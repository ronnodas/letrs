use std::iter::repeat_n;

use itertools::izip;

use crate::layout::{HorizontalLayout, LayoutMode, VerticalLayout};
use crate::str_ext::StrExt as _;
use crate::{Font, Hardblank, Header, PrintDirection};

#[derive(Debug)]
pub struct Renderer<'font> {
    font: &'font Font,
    settings: RenderSettings,
}

impl<'font> Renderer<'font> {
    pub(crate) const fn new(font: &'font Font) -> Self {
        let settings = RenderSettings::from_header(font.header());
        Self { font, settings }
    }

    pub(crate) fn render(&self, string: &str) -> String {
        if self.settings.full_width() {
            return self.full_width_render(string);
        }
        let mut buffer: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        if self.settings.print_direction == PrintDirection::LeftToRight {
            for character in string.chars() {
                width = self.appleft(&mut buffer, width, character);
            }
        } else {
            for character in string.chars().rev() {
                width = self.appleft(&mut buffer, width, character);
            }
        }
        //TODO proper vertical smushing
        if !self.settings.full_height() {
            buffer = buffer
                .into_iter()
                .skip_while(|row| Self::is_blank(row.iter().copied()))
                .collect();
            while buffer
                .last()
                .is_some_and(|row| Self::is_blank(row.iter().copied()))
            {
                drop(buffer.pop());
            }
        }
        let left_trim = buffer
            .iter()
            .map(|row| row.iter().rev().take_while(|&&c| c == ' ').count())
            .min()
            .unwrap_or(0);
        for row in &mut buffer {
            row.truncate(row.len() - left_trim);
        }
        self.join_and_replace_hard_blanks(buffer)
    }

    fn appleft(&self, buffer: &mut Vec<Vec<char>>, mut width: usize, character: char) -> usize {
        let Some(character) = self.font.get(character) else {
            return width;
        };
        let smush_data = self.smush_data(buffer, &character.rows);
        let shift = smush_data
            .iter()
            .map(|row| row.shift(width, character.width))
            .min()
            .unwrap_or_else(|| width.min(character.width));
        for (buffer_row, char_row, smush) in izip!(buffer, &character.rows, smush_data) {
            smush.combine(shift, buffer_row, char_row);
        }
        width += character.width;
        width -= shift;
        width
    }

    fn smush_data(&self, buffer: &[Vec<char>], char_rows: &[String]) -> Vec<RowSmush> {
        buffer
            .iter()
            .map(|s| s.iter().copied().rev().enumerate().find(|&(_, c)| c != ' '))
            .zip(char_rows.iter().map(|row| row.first_non_blank()))
            .map(|(left, right)| {
                RowSmush::new(left, right, self.settings, self.font.header.hardblank)
            })
            .collect()
    }

    fn full_width_render(&self, string: &str) -> String {
        let mut buffer = vec![String::new(); self.font.header.height];
        if self.settings.print_direction == PrintDirection::LeftToRight {
            for char in string.chars() {
                self.appleft_full_width(&mut buffer, char);
            }
        } else {
            for char in string.chars().rev() {
                self.appleft_full_width(&mut buffer, char);
            }
        }

        //TODO proper vertical smushing
        if !self.settings.full_height() {
            buffer = buffer
                .into_iter()
                .skip_while(|row| Self::is_blank(row.chars()))
                .collect();
            while buffer.last().is_some_and(|row| Self::is_blank(row.chars())) {
                drop(buffer.pop());
            }
        }
        self.join_and_replace_hard_blanks(buffer.iter().map(|row| row.chars()))
    }

    fn appleft_full_width(&self, buffer: &mut [String], char: char) {
        let Some(char) = self.font.get(char) else {
            return;
        };
        for (buf_row, char_row) in buffer.iter_mut().zip(&char.rows) {
            buf_row.push_str(char_row);
        }
    }

    fn join_and_replace_hard_blanks(
        &self,
        rows: impl IntoIterator<Item: IntoIterator<Item = char>>,
    ) -> String {
        let mut rows = rows.into_iter().map(|row| {
            row.into_iter().map(|char| {
                if self.font.header.hardblank == char {
                    ' '
                } else {
                    char
                }
            })
        });
        let Some(first_row) = rows.next() else {
            return String::new();
        };
        let mut buffer: String = first_row.collect();
        for row in rows {
            buffer.push('\n');
            buffer.extend(row);
        }
        buffer
    }

    fn is_blank(mut row: impl Iterator<Item = char>) -> bool {
        row.all(|c| c == ' ')
    }

    #[must_use]
    pub const fn print_direction(mut self, direction: PrintDirection) -> Self {
        self.settings.print_direction = direction;
        self
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RenderSettings {
    horizontal_layout: HorizontalLayout,
    vertical_layout: VerticalLayout,
    print_direction: PrintDirection,
    // TODO width
    // TODO alignment
}

impl RenderSettings {
    const fn from_header(header: &Header) -> Self {
        let horizontal_layout = header.horizontal_layout;
        let vertical_layout = header.vertical_layout;
        let print_direction = header.print_direction;
        Self {
            horizontal_layout,
            vertical_layout,
            print_direction,
        }
    }

    fn full_width(self) -> bool {
        self.horizontal_layout.mode() == LayoutMode::FullSize
    }

    fn full_height(self) -> bool {
        self.vertical_layout.mode() == LayoutMode::FullSize
    }
}

#[derive(Debug)]
enum RowSmush {
    BothEmpty,
    Keep {
        left_offset: usize,
    },
    Overwrite {
        right_offset: usize,
    },
    Smush {
        left_offset: usize,
        right_offset: usize,
        smush: Option<char>,
    },
}

impl RowSmush {
    fn shift(&self, left: usize, right: usize) -> usize {
        match self {
            Self::BothEmpty => left + right,
            Self::Keep { left_offset } => left_offset + right,
            Self::Overwrite { right_offset } => left + right_offset,
            Self::Smush {
                left_offset,
                right_offset,
                smush,
            } => left_offset + right_offset + usize::from(smush.is_some()),
        }
    }

    fn combine(&self, shift: usize, buffer_row: &mut Vec<char>, char_row: &str) {
        match self {
            Self::BothEmpty | Self::Keep { .. } => {
                if char_row.len() <= shift {
                    buffer_row.truncate(buffer_row.len() + char_row.len() - shift);
                } else {
                    buffer_row.extend(repeat_n(' ', char_row.len() - shift));
                }
            }
            Self::Overwrite { .. } => {
                buffer_row.truncate(buffer_row.len().saturating_sub(shift));
                let skip = shift.saturating_sub(buffer_row.len());
                buffer_row.extend(char_row.chars().skip(skip));
            }
            &Self::Smush {
                left_offset,
                right_offset,
                smush: Some(smush),
            } if shift > (right_offset + left_offset) => {
                // shift == self.shift()
                buffer_row.truncate(buffer_row.len() - left_offset - 1);
                buffer_row.push(smush);
                buffer_row.extend(char_row.chars().skip(right_offset + 1));
            }
            &Self::Smush { right_offset, .. } => {
                if shift <= right_offset {
                    buffer_row.extend(char_row.chars().skip(shift));
                } else {
                    buffer_row.truncate(buffer_row.len() + right_offset - shift);
                    buffer_row.extend(char_row.chars().skip(right_offset));
                }
            }
        }
    }

    fn new(
        left: Option<(usize, char)>,
        right: Option<(usize, char)>,
        settings: RenderSettings,
        hardblank: Hardblank,
    ) -> Self {
        match (left, right) {
            (None, None) => Self::BothEmpty,
            (None, Some((right_offset, _))) => Self::Overwrite { right_offset },
            (Some((left_offset, _)), None) => Self::Keep { left_offset },
            (Some((left_offset, left_char)), Some((right_offset, right_char))) => {
                let smush = if settings.print_direction == PrintDirection::LeftToRight {
                    settings
                        .horizontal_layout
                        .smush(left_char, right_char, hardblank)
                } else {
                    settings
                        .horizontal_layout
                        .smush(right_char, left_char, hardblank)
                };
                Self::Smush {
                    left_offset,
                    right_offset,
                    smush,
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::render::Renderer;
    use crate::{Font, PrintDirection};

    #[test]
    fn hello() {
        let rendered = Font::standard().render("hello");
        let expected = r" _          _ _       
| |__   ___| | | ___  
| '_ \ / _ \ | |/ _ \ 
| | | |  __/ | | (_) |
|_| |_|\___|_|_|\___/ ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn hello_world() {
        let rendered = Font::standard().render("Hello, world!");
        let expected = r" _   _      _ _                             _     _ _ 
| | | | ___| | | ___    __      _____  _ __| | __| | |
| |_| |/ _ \ | |/ _ \   \ \ /\ / / _ \| '__| |/ _` | |
|  _  |  __/ | | (_) |   \ V  V / (_) | |  | | (_| |_|
|_| |_|\___|_|_|\___( )   \_/\_/ \___/|_|  |_|\__,_(_)
                    |/                                ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn hello_world_flipped() {
        let rendered = Renderer::new(&Font::standard())
            .print_direction(PrintDirection::RightToLeft)
            .render("Hello, world!");
        let expected = r" _     _ _                            _ _      _   _ 
| | __| | |_ __ _____      __    ___ | | | ___| | | |
| |/ _` | | '__/ _ \ \ /\ / /   / _ \| | |/ _ \ |_| |
|_| (_| | | | | (_) \ V  V /   | (_) | | |  __/  _  |
(_)\__,_|_|_|  \___/ \_/\_/   ( )___/|_|_|\___|_| |_|
                              |/                     ";
        assert_eq!(rendered, expected);
    }
}
