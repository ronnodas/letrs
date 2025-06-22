use std::cmp::Ordering;
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
    #[must_use]
    pub fn new(font: &'font Font) -> Self {
        let settings = RenderSettings::from_header(font.header());
        Self { font, settings }
    }

    #[must_use]
    pub fn render(&self, mut string: &str) -> String {
        use std::mem::take;

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
            debug_assert!(
                self.settings.width.is_none() || line_width <= width,
                "rendered line too wide"
            );
            width = width.max(line_width);
            string = rest;
        }
        for row in lines.iter_mut().flatten() {
            for c in row.iter_mut() {
                if self.font.header.hardblank == *c {
                    *c = ' ';
                }
            }
            *row = self.settings.alignment.pad(take(row), width);
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
    pub const fn vertical_layout(mut self, mode: LayoutMode) -> Self {
        self.settings.vertical_layout.set_mode(mode);
        self
    }

    #[must_use]
    pub const fn alignment(mut self, alignment: Alignment) -> Self {
        self.settings.alignment = alignment;
        self
    }

    #[must_use]
    pub const fn max_width(mut self, width: usize) -> Self {
        self.settings.width = Some(width);
        self
    }

    fn render_line<'a>(&self, mut string: &'a str) -> (Vec<Vec<char>>, usize, &'a str) {
        let mut line: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        let mut chars = string.chars();
        let mut before_space = None;
        let mut overfull = false;
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if c == ' ' {
                before_space = Some((line.clone(), width, string));
            }
            if LINE_BREAK_CHARACTERS.contains(&c) {
                string = chars.as_str();
                break;
            }
            let appended = self.append(&mut line, &mut width, c);
            if !appended {
                overfull = true;
                break;
            }
            string = chars.as_str();
        }
        if overfull {
            if let Some(saved) = before_space {
                (line, width, string) = saved;
                string = string.trim_start_matches([' ', '\t']);
            }
        }
        let end_trim = Self::end_trimming(&line);
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
        //TODO consider trim here
        if self
            .settings
            .width
            .is_some_and(|max_width| new_width > max_width)
        {
            return false;
        }
        *width = new_width;
        for (buffer_row, char_row, smush) in izip!(line, &character.rows, smush_data) {
            smush.combine(shift, buffer_row, char_row, self.settings.direction);
        }
        true
    }

    fn render_line_full_width<'a>(&self, mut string: &'a str) -> (Vec<Vec<char>>, usize, &'a str) {
        let mut line: Vec<Vec<char>> = vec![Vec::new(); self.font.header.height];
        let mut width = 0;
        let mut chars = string.chars();
        let mut before_space = None;
        let mut overfull = false;
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if c == ' ' {
                before_space = Some((line.clone(), width, string));
            }
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
                overfull = true;
                break;
            }
            width += character.width;
            for (buf_row, char_row) in line.iter_mut().zip(&character.rows) {
                buf_row.extend(char_row.bidi_chars(self.settings.direction));
            }

            string = chars.as_str();
        }
        if overfull {
            if let Some(saved) = before_space {
                (line, width, string) = saved;
                string = string.trim_start_matches([' ', '\t']);
            }
        }
        (line, width, string)
    }

    fn row_smush_data(&self, buffer: &[Vec<char>], char_rows: &[String]) -> Vec<RowSmush> {
        buffer
            .iter()
            .zip(char_rows)
            .map(|(end, start)| {
                let end = RowSmush::count_blanks(end.iter().rev().copied());
                let start = RowSmush::count_blanks(start.bidi_chars(self.settings.direction));
                RowSmush::new(end, start, self.settings, self.font.header.hardblank)
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
                buffer.extend(row.into_iter().rev());
            }
            first = false;
        }
        buffer
    }

    fn stack(&self, lines: Vec<Vec<Vec<char>>>, width: usize) -> Vec<Vec<char>> {
        let mut rows = Vec::new();
        if self.settings.full_height() {
            rows.extend(lines.into_iter().flatten());
        } else {
            for line in lines {
                // Performance-wise, it might be better to transpose rows and line
                let smush_data = self.column_smush_data(&rows, &line, width);
                let shift = smush_data
                    .iter()
                    .map(|column| column.shift(rows.len(), line.len()))
                    .min()
                    .unwrap_or_else(|| rows.len().min(line.len()));
                for (i, smush) in smush_data.into_iter().enumerate() {
                    smush.combine(&mut rows, &line, i, shift);
                }
                if shift > line.len() {
                    rows.truncate(rows.len() + line.len() - shift);
                }
                rows.extend(line.into_iter().skip(shift));
            }
        }
        if !self.settings.full_height() {
            while rows.last().is_some_and(|row| row.iter().all(|&c| c == ' ')) {
                drop(rows.pop());
            }
        }
        rows
    }

    fn column_smush_data(
        &self,
        end: &[Vec<char>],
        start: &[Vec<char>],
        width: usize,
    ) -> Vec<ColumnSmush> {
        (0..width)
            .map(|i| {
                let end_counts = BlanksAndBars::count(end.iter().rev().map(|row| row[i]));
                let start_counts = BlanksAndBars::count(start.iter().map(|row| row[i]));
                ColumnSmush::new(&end_counts, &start_counts, self.settings.vertical_layout)
            })
            .collect()
    }

    fn end_trimming(line: &[Vec<char>]) -> usize {
        line.iter()
            .map(|row| row.iter().rev().take_while(|&&c| c == ' ').count())
            .min()
            .unwrap_or(0)
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

/// The choice of rendering alignment.
///
/// The alignment is defined relative to the printing direction,
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
    fn pad(self, mut row: Vec<char>, to_width: usize) -> Vec<char> {
        let Some(padding) = to_width.checked_sub(row.len()) else {
            return row;
        };
        match self {
            Self::Start => {
                row.extend(repeat_n(' ', padding));
                row
            }
            Self::Center => {
                let start = padding / 2;
                repeat_n(' ', start)
                    .chain(row)
                    .chain(repeat_n(' ', padding - start))
                    .collect()
            }
            Self::End => repeat_n(' ', padding).chain(row).collect(),
        }
    }
}

#[derive(Debug)]
enum RowSmush {
    Keep {
        end_blanks: usize,
    },
    Overwrite {
        start_blanks: usize,
    },
    Smush {
        end_blanks: usize,
        start_blanks: usize,
        smush: Option<char>,
    },
}

impl RowSmush {
    fn new(
        end: (usize, Option<char>),
        start: (usize, Option<char>),
        settings: RenderSettings,
        hardblank: Hardblank,
    ) -> Self {
        match (end.1, start.1) {
            (None, _) => Self::Overwrite {
                start_blanks: start.0,
            },
            (_, None) => Self::Keep { end_blanks: end.0 },
            (Some(end_char), Some(start_char)) => {
                let smush = settings
                    .horizontal_layout
                    .smush(end_char, start_char, hardblank);
                Self::Smush {
                    end_blanks: end.0,
                    start_blanks: start.0,
                    smush,
                }
            }
        }
    }

    fn shift(&self, end: usize, start: usize) -> usize {
        match self {
            Self::Keep { end_blanks } => end_blanks + start,
            Self::Overwrite { start_blanks } => end + start_blanks,
            Self::Smush {
                end_blanks,
                start_blanks,
                smush,
            } => end_blanks + start_blanks + usize::from(smush.is_some()),
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
            Self::Keep { .. } => {
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
                end_blanks,
                start_blanks,
                smush: Some(smush),
            } if shift > (start_blanks + end_blanks) => {
                // shift == self.shift()
                buffer_row.truncate(buffer_row.len() - end_blanks - 1);
                buffer_row.push(smush);
                buffer_row.extend(char_row.bidi_chars(direction).skip(start_blanks + 1));
            }
            &Self::Smush { start_blanks, .. } => {
                if shift <= start_blanks {
                    buffer_row.extend(char_row.bidi_chars(direction).skip(shift));
                } else {
                    buffer_row.truncate(buffer_row.len() + start_blanks - shift);
                    buffer_row.extend(char_row.bidi_chars(direction).skip(start_blanks));
                }
            }
        }
    }

    fn count_blanks(chars: impl Iterator<Item = char>) -> (usize, Option<char>) {
        let mut blanks = 0;
        let mut next = None;
        for c in chars {
            if c == ' ' {
                blanks += 1;
            } else {
                next = Some(c);
                break;
            }
        }
        (blanks, next)
    }
}

enum ColumnSmush {
    Keep {
        end_offset: usize,
    },
    Overwrite {
        start_offset: usize,
    },
    NoBars {
        end_blanks: usize,
        start_blanks: usize,
        smush: Option<char>,
    },
    EndBar {
        end_blanks: usize,
        start_blanks: usize,
        start_offset: usize,
        smush: Option<char>,
    },
    StartBar {
        smush: Option<char>,
        end_offset: usize,
        end_blanks: usize,
        start_blanks: usize,
    },
    DoubleSmush {
        end_blanks: usize,
        bars: usize,
        start_blanks: usize,
        smush: Option<(char, char)>,
    },
}

impl ColumnSmush {
    fn new(end: &BlanksAndBars, start: &BlanksAndBars, layout: VerticalLayout) -> Self {
        if layout.super_smushing() {
            match (end.next, end.bars, start.bars, start.next) {
                (_, _, 0, None) => Self::Keep {
                    end_offset: end.total(),
                },
                (None, 0, _, _) => Self::Overwrite {
                    start_offset: start.total(),
                },
                (Some(end_next), 0, 0, Some(start_next)) => Self::NoBars {
                    end_blanks: end.blanks,
                    start_blanks: start.blanks,
                    smush: layout.smush(end_next, start_next),
                },
                _ => match end.bars.cmp(&start.bars) {
                    Ordering::Less => Self::StartBar {
                        smush: end.next.and_then(|end| layout.smush(end, '|')),
                        end_offset: end.total(),
                        end_blanks: end.blanks,
                        start_blanks: start.blanks,
                    },
                    Ordering::Equal => {
                        let smush = end
                            .next
                            .and_then(|end| layout.smush(end, '|'))
                            .zip(start.next.and_then(|start| layout.smush('|', start)));
                        Self::DoubleSmush {
                            end_blanks: end.blanks,
                            bars: end.bars,
                            start_blanks: start.blanks,
                            smush,
                        }
                    }
                    Ordering::Greater => Self::EndBar {
                        end_blanks: end.blanks,
                        start_blanks: start.blanks,
                        start_offset: start.total(),
                        smush: start.next.and_then(|start| layout.smush('|', start)),
                    },
                },
            }
        } else {
            match (end.next_to_blank(), start.next_to_blank()) {
                (_, None) => Self::Keep {
                    end_offset: end.blanks,
                },
                (None, Some(_)) => Self::Overwrite {
                    start_offset: start.blanks,
                },
                (Some(end_next), Some(start_next)) => Self::NoBars {
                    end_blanks: end.blanks,
                    start_blanks: start.blanks,
                    smush: layout.smush(end_next, start_next),
                },
            }
        }
    }

    fn shift(&self, end: usize, start: usize) -> usize {
        match self {
            Self::Keep { end_offset } => end_offset + start,
            Self::Overwrite { start_offset } => start_offset + end,
            Self::NoBars {
                end_blanks,
                start_blanks,
                smush,
            } => end_blanks + start_blanks + usize::from(smush.is_some()),
            Self::EndBar {
                end_blanks,
                start_offset,
                smush,
                ..
            } => end_blanks + start_offset + usize::from(smush.is_some()),
            Self::StartBar {
                smush,
                end_offset,
                start_blanks,
                ..
            } => end_offset + start_blanks + usize::from(smush.is_some()),
            Self::DoubleSmush {
                end_blanks,
                bars,
                start_blanks,
                smush,
            } => end_blanks + bars + start_blanks + usize::from(smush.is_some()),
        }
    }

    fn combine(&self, rows: &mut [Vec<char>], line: &[Vec<char>], i: usize, shift: usize) {
        // this is mostly shift.saturating_sub(start_blanks).min(end_blanks) and the special case
        // end_blanks when smushing occurs but it's not clear how to avoid the repetition
        let rows_to_overwrite = match self {
            Self::Keep { .. } => return,
            &Self::Overwrite { .. } => rows.len().min(shift),
            &Self::NoBars {
                end_blanks,
                start_blanks,
                smush,
            } => {
                if shift > end_blanks + start_blanks {
                    let smush = smush.expect("shift <= ... + smush.is_some()");
                    rows[rows.len() - end_blanks - 1][i] = smush;
                    end_blanks
                } else {
                    shift.saturating_sub(start_blanks)
                }
            }
            &Self::EndBar {
                end_blanks,
                start_blanks,
                start_offset,
                smush,
            } => {
                if shift > end_blanks + start_offset {
                    let smush = smush.expect("shift <= ... + smush.is_some()");
                    rows[rows.len() - end_blanks - 1][i] = smush;
                    end_blanks
                } else {
                    shift.saturating_sub(start_blanks).min(end_blanks)
                }
            }
            &Self::StartBar {
                smush,
                end_offset,
                end_blanks,
                start_blanks,
            } => {
                if shift > end_offset + start_blanks {
                    let smush = smush.expect("shift <= ... + smush.is_some()");
                    rows[rows.len() - end_offset - 1][i] = smush;
                    end_blanks
                } else {
                    shift.saturating_sub(start_blanks).min(end_blanks)
                }
            }
            &Self::DoubleSmush {
                end_blanks,
                bars,
                start_blanks,
                smush,
            } => {
                if shift > end_blanks + bars + start_blanks {
                    let (row_bar_smush, line_bar_smush) =
                        smush.expect("shift <= ... + smush.is_some()");
                    rows[rows.len() - end_blanks - bars - 1][i] = row_bar_smush;
                    rows[rows.len() - end_blanks - 1][i] = line_bar_smush;
                    end_blanks
                } else {
                    shift.saturating_sub(start_blanks).min(end_blanks)
                }
            }
        };
        let fixed_rows = rows.len() - rows_to_overwrite;
        rows.iter_mut()
            .skip(fixed_rows)
            .zip(line.iter().skip(shift - rows_to_overwrite))
            .for_each(|(row, line_row)| row[i] = line_row[i]);
    }
}

struct BlanksAndBars {
    blanks: usize,
    bars: usize,
    next: Option<char>,
}

impl BlanksAndBars {
    fn count(chars: impl Iterator<Item = char>) -> Self {
        let mut blanks = 0;
        let mut bars = 0;
        let mut next = None;
        let mut in_bars = false;
        for c in chars {
            match (c, in_bars) {
                (' ', false) => blanks += 1,
                ('|', false) => {
                    bars += 1;
                    in_bars = true;
                }
                ('|', true) => bars += 1,
                (c, _) => {
                    next = Some(c);
                    break;
                }
            }
        }
        Self { blanks, bars, next }
    }

    const fn total(&self) -> usize {
        self.blanks + self.bars
    }

    const fn next_to_blank(&self) -> Option<char> {
        if self.bars > 0 { Some('|') } else { self.next }
    }
}

#[cfg(test)]
mod test {
    use crate::{Font, PrintDirection};

    use super::{Alignment, Renderer};

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

    #[test]
    fn multi_line() {
        let rendered = Font::standard().render("Hello\n  world!");
        let expected = r" _   _      _ _                 
| | | | ___| | | ___            
| |_| |/ _ \ | |/ _ \           
|  _  |  __/ | | (_) |_     _ _ 
|_|_|_|\___|_|_|\___/| | __| | |
  \ \ /\ / / _ \| '__| |/ _` | |
   \ V  V / (_) | |  | | (_| |_|
    \_/\_/ \___/|_|  |_|\__,_(_)";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn center_align() {
        let rendered = Renderer::new(&Font::standard())
            .alignment(Alignment::Center)
            .render("short\ncomparatively long");
        let expected = r"                                    _                _                                   
                                ___| |__   ___  _ __| |_                                 
                               / __| '_ \ / _ \| '__| __|                                
                               \__ \ | | | (_) | |  | |_                                 
                               |___/_| |_|\___/|_|   \__|  _         _                   
  ___ ___  _ __ ___  _ __   __ _ _ __ __ _| |_(_)_   _____| |_   _  | | ___  _ __   __ _ 
 / __/ _ \| '_ ` _ \| '_ \ / _` | '__/ _` | __| \ \ / / _ \ | | | | | |/ _ \| '_ \ / _` |
| (_| (_) | | | | | | |_) | (_| | | | (_| | |_| |\ V /  __/ | |_| | | | (_) | | | | (_| |
 \___\___/|_| |_| |_| .__/ \__,_|_|  \__,_|\__|_| \_/ \___|_|\__, | |_|\___/|_| |_|\__, |
                    |_|                                      |___/                 |___/ ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn end_align() {
        let rendered = Renderer::new(&Font::standard())
            .alignment(Alignment::End)
            .render("short\ncomparatively long");
        let expected = r"                                                                    _                _   
                                                                ___| |__   ___  _ __| |_ 
                                                               / __| '_ \ / _ \| '__| __|
                                                               \__ \ | | | (_) | |  | |_ 
                                           _   _           _   |___/_| |_|\___/|_|   \__|
  ___ ___  _ __ ___  _ __   __ _ _ __ __ _| |_(_)_   _____| |_   _  | | ___  _ __   __ _ 
 / __/ _ \| '_ ` _ \| '_ \ / _` | '__/ _` | __| \ \ / / _ \ | | | | | |/ _ \| '_ \ / _` |
| (_| (_) | | | | | | |_) | (_| | | | (_| | |_| |\ V /  __/ | |_| | | | (_) | | | | (_| |
 \___\___/|_| |_| |_| .__/ \__,_|_|  \__,_|\__|_| \_/ \___|_|\__, | |_|\___/|_| |_|\__, |
                    |_|                                      |___/                 |___/ ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn forced_line_break_no_space() {
        let rendered = Renderer::new(&Font::standard())
            .max_width(80)
            .render("floccinaucinihilipilification");
        let expected = r"  __ _                _                        _       _ _     _ _ _       _ _  
 / _| | ___   ___ ___(_)_ __   __ _ _   _  ___(_)_ __ (_) |__ (_) (_)_ __ (_) | 
| |_| |/ _ \ / __/ __| | '_ \ / _` | | | |/ __| | '_ \| | '_ \| | | | '_ \| | | 
|  _| | (_) | (_| (__| | | | | (_| | |_| | (__| | | | | | | | | | | | |_) | | | 
|_| |_|\___/ \___\___|_|_| |_|\__,_|\__,_|\___|_|_| |_|_|_| |_|_|_|_| .__/|_|_| 
(_)/ _(_) ___ __ _| |_(_) ___  _ __                                 |_|         
| | |_| |/ __/ _` | __| |/ _ \| '_ \                                            
| |  _| | (_| (_| | |_| | (_) | | | |                                           
|_|_| |_|\___\__,_|\__|_|\___/|_| |_|                                           ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn forced_line_break_lots_of_spaces() {
        let rendered = Renderer::new(&Font::standard())
            .max_width(80)
            .render("floccinauci                  nihilipilification");
        let expected = r"  __ _                _                        _                                
 / _| | ___   ___ ___(_)_ __   __ _ _   _  ___(_)                               
| |_| |/ _ \ / __/ __| | '_ \ / _` | | | |/ __| |                               
|  _| | (_) | (_| (__| | | | | (_| | |_| | (__| |                               
|_| |_|\___/ \___\___|_|_| |_|\__,_|\__,_|\___|_|_   _                          
 _ __ (_) |__ (_) (_)_ __ (_) (_)/ _(_) ___ __ _| |_(_) ___  _ __               
| '_ \| | '_ \| | | | '_ \| | | | |_| |/ __/ _` | __| |/ _ \| '_ \              
| | | | | | | | | | | |_) | | | |  _| | (_| (_| | |_| | (_) | | | |             
|_| |_|_|_| |_|_|_|_| .__/|_|_|_|_| |_|\___\__,_|\__|_|\___/|_| |_|             
                    |_|                                                         ";
        assert_eq!(rendered, expected);
    }
}
