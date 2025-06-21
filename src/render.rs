use std::iter::repeat_n;

use itertools::izip;

use crate::str_ext::StrExt as _;
use crate::{Font, Header};

#[derive(Debug)]
pub struct Renderer<'font> {
    font: &'font Font,
}

impl<'font> Renderer<'font> {
    pub(crate) const fn new(font: &'font Font) -> Self {
        Self { font }
    }

    pub(crate) fn render(&self, string: &str) -> String {
        if self.font.header.horizontal_layout.is_full_size() {
            return self.full_width_render(string);
        }
        //TODO use print_direction
        let mut buffer: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        for character in string.chars() {
            let Some(character) = self.font.get(character) else {
                continue;
            };
            let smush_data = self.smush_data(&buffer, &character.rows);
            let shift = smush_data
                .iter()
                .map(|row| row.shift(width, character.width))
                .min()
                .unwrap_or_else(|| width.min(character.width));

            for (buffer_row, char_row, smush) in izip!(&mut buffer, &character.rows, smush_data) {
                smush.combine(shift, buffer_row, char_row)
            }
            width += character.width;
            width -= shift;
        }
        //TODO proper vertical smushing
        if !self.font.header.vertical_layout.is_full_size() {
            buffer = buffer
                .into_iter()
                .skip_while(|row| Self::is_blank(row.iter().copied()))
                .collect();
            while buffer
                .last()
                .is_some_and(|row| Self::is_blank(row.iter().copied()))
            {
                _ = buffer.pop();
            }
        }
        let end_trim = buffer
            .iter()
            .map(|row| row.iter().rev().take_while(|&&c| c == ' ').count())
            .min()
            .unwrap_or(0);
        for row in &mut buffer {
            row.truncate(row.len() - end_trim)
        }
        self.join_and_replace_hard_blanks(buffer)
    }

    fn smush_data(&self, buffer: &[Vec<char>], char_rows: &[String]) -> Vec<RowSmush> {
        buffer
            .iter()
            .map(|s| s.iter().copied().rev().enumerate().find(|&(_, c)| c != ' '))
            .zip(char_rows.iter().map(|row| row.first_non_blank()))
            .map(|(end, start)| RowSmush::new(end, start, self.font.header()))
            .collect()
    }

    fn full_width_render(&self, string: &str) -> String {
        //TODO use print_direction
        let mut buffer = vec![String::new(); self.font.header.height];
        for char in string.chars() {
            let Some(char) = self.font.get(char) else {
                continue;
            };
            for (buf_row, char_row) in buffer.iter_mut().zip(&char.rows) {
                buf_row.push_str(char_row);
            }
        }
        //TODO proper vertical smushing
        if !self.font.header.vertical_layout.is_full_size() {
            buffer = buffer
                .into_iter()
                .skip_while(|row| Self::is_blank(row.chars()))
                .collect();
            while buffer.last().is_some_and(|row| Self::is_blank(row.chars())) {
                _ = buffer.pop();
            }
        }
        self.join_and_replace_hard_blanks(buffer.iter().map(|row| row.chars()))
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
}

#[derive(Debug)]
enum RowSmush {
    BothEmpty,
    Keep {
        end_offset: usize,
    },
    Overwrite {
        start_offset: usize,
    },
    Smush {
        end_offset: usize,
        start_offset: usize,
        smush: Option<char>,
    },
}

impl RowSmush {
    fn shift(&self, end: usize, start: usize) -> usize {
        match self {
            Self::BothEmpty => end + start,
            Self::Keep { end_offset } => end_offset + start,
            Self::Overwrite { start_offset } => end + start_offset,
            Self::Smush {
                end_offset,
                start_offset,
                smush,
            } => end_offset + start_offset + usize::from(smush.is_some()),
        }
    }

    fn combine(&self, shift: usize, buffer_row: &mut Vec<char>, char_row: &str) {
        match self {
            Self::BothEmpty | Self::Keep { .. } => {
                if char_row.len() <= shift {
                    buffer_row.truncate(buffer_row.len() + char_row.len() - shift);
                } else {
                    buffer_row.extend(repeat_n(' ', char_row.len() - shift))
                }
            }
            Self::Overwrite { .. } => {
                buffer_row.truncate(buffer_row.len().saturating_sub(shift));
                let skip = shift.saturating_sub(buffer_row.len());
                buffer_row.extend(char_row.chars().skip(skip));
            }
            &Self::Smush {
                end_offset,
                start_offset,
                smush: Some(smush),
            } if shift > (start_offset + end_offset) => {
                // shift == self.shift()
                buffer_row.truncate(buffer_row.len() - end_offset - 1);
                buffer_row.push(smush);
                buffer_row.extend(char_row.chars().skip(start_offset + 1));
            }
            &Self::Smush { start_offset, .. } => {
                if shift <= start_offset {
                    buffer_row.extend(char_row.chars().skip(shift));
                } else {
                    buffer_row.truncate(buffer_row.len() + start_offset - shift);
                    buffer_row.extend(char_row.chars().skip(start_offset));
                }
            }
        }
    }

    fn new(end: Option<(usize, char)>, start: Option<(usize, char)>, header: &Header) -> Self {
        match (end, start) {
            (None, None) => Self::BothEmpty,
            (None, Some((start_offset, _))) => Self::Overwrite { start_offset },
            (Some((end_offset, _)), None) => Self::Keep { end_offset },
            (Some((end_offset, end_char)), Some((start_offset, start_char))) => {
                let smush = header
                    .horizontal_layout
                    .smush(end_char, start_char, header.hardblank);
                Self::Smush {
                    end_offset,
                    start_offset,
                    smush,
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Font;

    #[test]
    fn hello() {
        let rendered = Font::standard().render("hello");
        let expected = r" _          _ _       
| |__   ___| | | ___  
| '_ \ / _ \ | |/ _ \ 
| | | |  __/ | | (_) |
|_| |_|\___|_|_|\___/ ";
        assert_eq!(rendered, expected)
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
        assert_eq!(rendered, expected)
    }
}
