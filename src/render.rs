use std::iter::repeat_n;

use itertools::izip;

use crate::layout::{HorizontalLayout, LayoutMode, VerticalLayout};
use crate::str_ext::StrExt as _;
use crate::{Font, Hardblank, Header, PrintDirection};

const LINE_BREAK_CHARACTERS: [char; 4] = ['\n', '\r', '\x11', '\x12'];

#[derive(Debug)]
pub struct Renderer<'font> {
    font: &'font Font,
    settings: RenderSettings,
}

impl<'font> Renderer<'font> {
    pub fn new(font: &'font Font) -> Self {
        let settings = RenderSettings::from_header(font.header());
        Self { font, settings }
    }

    pub fn render(&self, mut string: &str) -> String {
        let mut lines: Vec<Vec<Vec<char>>> = Vec::new();
        let mut width = self.settings.width.unwrap_or(0);
        while !string.is_empty() {
            let (line, line_width, rest) = if self.settings.full_width() {
                self.render_line_full_width(string)
            } else {
                self.render_line(string)
            };
            if rest == string {
                todo!("probably width too small")
            }
            lines.push(line);
            debug_assert!(self.settings.width.is_none() || line_width <= width);
            width = width.max(line_width);
            string = rest;
        }
        for row in lines.iter_mut().flatten() {
            for c in row.iter_mut() {
                if self.font.header.hardblank == *c {
                    *c = ' '
                }
            }
            self.settings.alignment.pad(row, width);
        }
        let rows = self.stack(lines, width);
        self.join(rows)
    }

    #[must_use]
    pub const fn print_direction(mut self, direction: PrintDirection) -> Self {
        self.settings.direction = direction;
        self
    }

    #[must_use]
    pub fn vertical_layout(mut self, mode: LayoutMode) -> Self {
        self.settings.vertical_layout.set_mode(mode);
        self
    }

    fn render_line<'a>(&self, mut string: &'a str) -> (Vec<Vec<char>>, usize, &'a str) {
        let mut line: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        let mut chars = string.chars();
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if LINE_BREAK_CHARACTERS.contains(&c) {
                string = chars.as_str();
                break;
            }
            let appended = self.append(&mut line, &mut width, c);
            if !appended {
                break;
            }
            string = chars.as_str();
        }

        let end_trim = line
            .iter()
            .map(|row| row.iter().rev().take_while(|&&c| c == ' ').count())
            .min()
            .unwrap_or(0);
        width -= end_trim;
        for row in &mut line {
            row.truncate(row.len() - end_trim);
        }
        (line, width, string)
    }

    fn append(&self, line: &mut Vec<Vec<char>>, width: &mut usize, character: char) -> bool {
        let Some(character) = self.font.get(character) else {
            return true;
        };
        let smush_data = self.row_smush_data(line, &character.rows);
        let shift = smush_data
            .iter()
            .map(|row| row.shift(*width, character.width))
            .min()
            .unwrap_or_else(|| (*width).min(character.width));
        let new_width = *width + character.width - shift;
        if self
            .settings
            .width
            .is_some_and(|max_width| new_width > max_width)
        {
            return false;
        } else {
            *width = new_width
        }
        for (buffer_row, char_row, smush) in izip!(line, &character.rows, smush_data) {
            smush.combine(shift, buffer_row, char_row, self.settings.direction);
        }
        true
    }

    fn render_line_full_width<'a>(&self, mut string: &'a str) -> (Vec<Vec<char>>, usize, &'a str) {
        let mut line: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        let mut chars = string.chars();
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if LINE_BREAK_CHARACTERS.contains(&c) {
                string = chars.as_str();
                break;
            }
            let Some(character) = self.font.get(c) else {
                continue;
            };
            if self
                .settings
                .width
                .is_some_and(|max_width| character.width + width > max_width)
            {
                break;
            } else {
                width += character.width;
            }
            for (buf_row, char_row) in line.iter_mut().zip(&character.rows) {
                buf_row.extend(char_row.bidi_chars(self.settings.direction));
            }

            string = chars.as_str();
        }
        (line, width, string)
    }

    fn row_smush_data(&self, buffer: &[Vec<char>], char_rows: &[String]) -> Vec<RowSmush> {
        buffer
            .iter()
            .map(|s| s.iter().copied().rev().enumerate().find(|&(_, c)| c != ' '))
            .zip(
                char_rows
                    .iter()
                    .map(|row| row.start_non_blank(self.settings.direction)),
            )
            .map(|(left, right)| {
                RowSmush::new(left, right, self.settings, self.font.header.hardblank)
            })
            .collect()
    }

    fn join(&self, rows: Vec<Vec<char>>) -> String {
        let mut buffer = String::new();
        let mut first = true;
        for row in rows {
            if !first {
                buffer.push('\n');
            }
            if self.settings.direction == PrintDirection::LeftToRight {
                buffer.extend(row);
            } else {
                buffer.extend(row.into_iter().rev())
            }
            first = false;
        }
        buffer
    }

    fn is_blank(mut row: impl Iterator<Item = char>) -> bool {
        row.all(|c| c == ' ')
    }

    fn stack(&self, lines: Vec<Vec<Vec<char>>>, width: usize) -> Vec<Vec<char>> {
        let mut rows = Vec::new();
        if self.settings.full_height() {
            rows.extend(lines.into_iter().flatten())
        } else {
            for line in lines {
                todo!()
                // let smush_data = self.column_smush_data(&rows, &line, width);
                // let shift = smush_data
                //     .iter()
                //     .map(|row| row.shift(*width, character.width))
                //     .min()
                //     .unwrap_or_else(|| (*width).min(character.width));
                // for (buffer_row, char_row, smush) in todo!() {
                //     smush.combine(shift, buffer_row, char_row, self.settings.direction);
                // }
            }
        }
        rows
    }
}

#[derive(Clone, Copy, Debug)]
pub struct RenderSettings {
    horizontal_layout: HorizontalLayout,
    vertical_layout: VerticalLayout,
    direction: PrintDirection,
    width: Option<usize>,
    alignment: Alignment,
}

impl RenderSettings {
    fn from_header(header: &Header) -> Self {
        Self {
            horizontal_layout: header.horizontal_layout,
            vertical_layout: header.vertical_layout,
            direction: header.print_direction,
            width: None,
            alignment: Alignment::default(),
        }
    }

    fn full_width(self) -> bool {
        self.horizontal_layout.mode() == LayoutMode::FullSize
    }

    fn full_height(self) -> bool {
        self.vertical_layout.mode() == LayoutMode::FullSize
    }
}

/// The choice of rendering alignment. The alignment is defined relative to the printing direction,
/// so `Alignment::Start` will align text on the left if printing left-to-right and on the right if
/// printing right-to-left.
///
/// This is only relevant if a maximum width is set or if rendering text with line breaks.
///
/// When the line width and the rendered text width have different parity (i.e. one is odd and the
/// other is even), `Alignment::Center` rounds so that there's one fewer blank in the 'start'
/// direction than the 'end'.
///
/// The default is `Alignment::Start`.
#[derive(Clone, Copy, Debug, Default)]
pub enum Alignment {
    #[default]
    Start,
    Center,
    End,
}

impl Alignment {
    fn pad(&self, row: &mut Vec<char>, to_width: usize) {
        use std::mem::take;

        let Some(padding) = to_width.checked_sub(row.len()) else {
            return;
        };
        match self {
            Self::Start => row.extend(repeat_n(' ', padding)),
            Self::Center => {
                let start = padding / 2;
                *row = repeat_n(' ', start)
                    .chain(take(row))
                    .chain(repeat_n(' ', padding - start))
                    .collect();
            }
            Self::End => *row = repeat_n(' ', padding).chain(take(row)).collect(),
        }
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

    fn combine(
        &self,
        shift: usize,
        buffer_row: &mut Vec<char>,
        char_row: &str,
        direction: PrintDirection,
    ) {
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
                buffer_row.extend(char_row.bidi_chars(direction).skip(skip));
            }
            &Self::Smush {
                left_offset,
                right_offset,
                smush: Some(smush),
            } if shift > (right_offset + left_offset) => {
                // shift == self.shift()
                buffer_row.truncate(buffer_row.len() - left_offset - 1);
                buffer_row.push(smush);
                buffer_row.extend(char_row.bidi_chars(direction).skip(right_offset + 1));
            }
            &Self::Smush { right_offset, .. } => {
                if shift <= right_offset {
                    buffer_row.extend(char_row.bidi_chars(direction).skip(shift));
                } else {
                    buffer_row.truncate(buffer_row.len() + right_offset - shift);
                    buffer_row.extend(char_row.bidi_chars(direction).skip(right_offset));
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
                let smush = if settings.direction == PrintDirection::LeftToRight {
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
