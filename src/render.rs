use itertools::izip;

use crate::Font;
use crate::str_ext::StrExt as _;

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
        let mut chars = string.chars();
        let Some(start_char) = chars.find_map(|char| self.font.get(char)) else {
            return String::new();
        };
        let mut buffer: Vec<Vec<char>> = start_char
            .rows
            .iter()
            .map(|row| row.chars().collect())
            .collect();
        let width = start_char.width;

        for character in chars {
            let Some(character) = self.font.get(character) else {
                continue;
            };
            let smush_data = self.smush_data(&buffer, &character.rows);
            let shift = smush_data
                .iter()
                .flatten()
                .map(RowSmush::shift)
                .min()
                .unwrap_or_else(|| width.min(character.width));
            for (buffer_row, char_row, shift_and_smush) in
                izip!(&mut buffer, &character.rows, smush_data)
            {
                match shift_and_smush {
                    Ok(smush_data) => smush_data.combine(shift, buffer_row, char_row),
                    Err(overwrite) => match overwrite {
                        Overwrite::Keep => {
                            if character.width < shift {
                                buffer_row.truncate(width + character.width - shift);
                            }
                        }
                        Overwrite::Overwrite => {
                            buffer_row.clear();
                            buffer_row.extend(char_row.chars().skip(shift.saturating_sub(width)));
                        }
                    },
                }
            }
        }
        self.join_and_replace_hard_blanks(buffer)
    }

    fn smush_data(
        &self,
        buffer: &[Vec<char>],
        char_rows: &[String],
    ) -> Vec<Result<RowSmush, Overwrite>> {
        buffer
            .iter()
            .map(|s| s.iter().copied().enumerate().rev().find(|&(_, c)| c != ' '))
            .zip(char_rows.iter().map(|row| row.first_non_blank()))
            .map(|(end, start)| {
                let Some((start_offset, start_char)) = start else {
                    return Err(Overwrite::Keep);
                };
                let Some((end_offset, end_char)) = end else {
                    return Err(Overwrite::Overwrite);
                };
                let smush = self.font.header.horizontal_layout.smush(
                    end_char,
                    start_char,
                    self.font.header.hardblank,
                );
                Ok(RowSmush {
                    end_offset,
                    start_offset,
                    smush,
                })
            })
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
}

enum Overwrite {
    Keep,
    Overwrite,
}

struct RowSmush {
    end_offset: usize,
    start_offset: usize,
    smush: Option<char>,
}

impl RowSmush {
    fn shift(&self) -> usize {
        self.end_offset + self.start_offset + usize::from(self.smush.is_some())
    }

    fn combine(&self, shift: usize, buffer_row: &mut Vec<char>, char_row: &str) {
        //TODO use let chain
        match self.smush {
            Some(smush) if shift > (self.start_offset + self.end_offset) => {
                // shift == self.shift()
                buffer_row.truncate(buffer_row.len() - self.end_offset - 1);
                buffer_row.push(smush);
                buffer_row.extend(char_row.chars().skip(self.start_offset + 1));
            }
            _ => {
                let skip = self.end_offset + self.start_offset - shift;
                if skip <= self.start_offset {
                    buffer_row.extend(char_row.chars().skip(skip));
                } else {
                    buffer_row.truncate(buffer_row.len() + self.start_offset - skip);
                    buffer_row.extend(char_row.chars().skip(self.start_offset));
                }
            }
        }
    }
}
