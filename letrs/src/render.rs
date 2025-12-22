//! Renderer and rendering settings
mod layout;

use std::cmp::Ordering;
use std::iter::repeat_n;
use std::mem;

use itertools::izip;

use crate::font::{Font, Hardblank, Header, PrintDirection};
use crate::str_ext::BStrExt as _;

pub use layout::{
    HorizontalLayout, HorizontalSmushing, Layout, LayoutDecodeError, LayoutMode, VerticalLayout,
    VerticalSmushing,
};

const LINE_BREAKS: [char; 4] = ['\n', '\r', '\x11', '\x12'];

/// The main type for rendering
///
/// Use [`render()`](Renderer::render) to process strings.
///
/// The other methods are meant to be used in a builder pattern:
/// ```
/// # use letrs::font::{Font, PrintDirection};
/// # use letrs::render::{Alignment, LayoutMode, Renderer};
/// let font = Font::standard();
/// let rendered: String = Renderer::new(&font)
///     .alignment(Alignment::Center)
///     .print_direction(PrintDirection::RightToLeft)
///     .horizontal_layout(LayoutMode::Fitting)
///     .vertical_layout(LayoutMode::FullSize)
///     .render("Hello,\nworld!");
/// let expected = concat!(
/// r"             _  _        _   _    ", "\n",
/// r"       ___  | || |  ___ | | | |   ", "\n",
/// r"      / _ \ | || | / _ \| |_| |   ", "\n",
/// r"    _| (_) || || ||  __/|  _  |   ", "\n",
/// r"   ( )\___/ |_||_| \___||_| |_|   ", "\n",
/// r"   |/                             ", "\n",
/// r" _      _  _                      ", "\n",
/// r"| |  __| || | _ __  ___ __      __", "\n",
/// r"| | / _` || || '__|/ _ \\ \ /\ / /", "\n",
/// r"|_|| (_| || || |  | (_) |\ V  V / ", "\n",
/// r"(_) \__,_||_||_|   \___/  \_/\_/  ", "\n",
/// r"                                  "
/// );
/// assert_eq!(rendered, expected);
/// ```
#[must_use]
#[derive(Debug, Clone, Copy)]
pub struct Renderer<'font, W> {
    font: &'font Font,
    config: Config,
    width: W,
}

impl<W: WidthConfig> Renderer<'_, W> {
    /// Sets the print direction.
    pub const fn print_direction(mut self, direction: PrintDirection) -> Self {
        self.config.direction = direction;
        self
    }

    /// Sets the alignment.
    pub const fn alignment(mut self, alignment: Alignment) -> Self {
        self.config.alignment = alignment;
        self
    }

    /// Sets the horizontal layout mode.
    pub const fn horizontal_layout(mut self, mode: LayoutMode) -> Self {
        self.config.horizontal_layout.set_mode(mode);
        self
    }

    /// Sets the vertical layout mode.
    pub const fn vertical_layout(mut self, mode: LayoutMode) -> Self {
        self.config.vertical_layout.set_mode(mode);
        self
    }

    /// Renders the given string.
    ///
    /// A newline (or carriage return, vertical tab, or form feed) always causes a line break. If
    /// `max_width` is set, any line that is too long is broken at the last contiguous segment of
    /// whitespace (spaces and tabs) if any, in which case that segment of whitespace is trimmed
    /// appropriately. If there is no such whitespace, then the line will be broken in the middle of
    /// a "word" (but never in the middle of a FIGcharacter), at the latest possible position.
    ///
    /// The output type can be:
    /// * [`Vec<u8>`]: recommended in case the font uses characters that are not valid UTF-8 (which
    ///   you can check using [`Font::is_utf8`]);
    /// * [`String`]: a convenience wrapper using [`String::from_utf8_lossy`] after rendering as
    ///   [`Vec<u8>`].
    pub fn render<Output: RenderOutput>(self, string: &str) -> W::Output<Output> {
        W::map(W::render_bytes(self, string))
    }

    fn render_line<'a>(&self, mut string: &'a str) -> (Vec<Vec<u8>>, usize, &'a str) {
        let mut line: Vec<Vec<u8>> = vec![Vec::new(); self.font.header().height.get()];
        let mut width = 0;
        let mut chars = string.chars();
        let mut before_space = None;
        let mut saved = false;
        let mut overfull = false;
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if c == ' ' && !saved {
                before_space = Some((line.clone(), width, string));
            }
            if LINE_BREAKS.contains(&c) {
                string = chars.as_str();
                break;
            }
            let appended = self.append(&mut line, &mut width, c);
            if !appended {
                overfull = true;
                break;
            }
            string = chars.as_str();
            if c != ' ' {
                saved = false;
            }
        }
        if overfull {
            if let Some(saved) = before_space {
                (line, width, string) = saved;
                string = string.trim_start_matches([' ', '\t']);
            }
        }
        let end_trim = Self::trimming(line.iter().map(|row| row.iter().rev().copied()));
        width -= end_trim;
        for row in &mut line {
            row.truncate(row.len() - end_trim);
        }
        (line, width, string)
    }

    fn append(&self, line: &mut Vec<Vec<u8>>, width: &mut usize, c: char) -> bool {
        let Some(character) = self.font.get(c) else {
            return true;
        };
        let smush_data = self.row_smush_data(line, &character.rows);
        let shift = smush_data
            .iter()
            .map(|row| row.shift(*width, character.width))
            .min()
            .unwrap_or_else(|| (*width).min(character.width));
        let char_rows_rev = character
            .rows
            .iter()
            .map(|row| row.bidi_chars(self.config.direction).rev());
        let trim = Self::trimming(char_rows_rev);
        if self
            .width
            .as_option()
            .is_some_and(|max_width| *width + character.width - trim - shift > max_width)
        {
            return false;
        }
        *width = *width + character.width - shift;
        for (buffer_row, char_row, smush) in izip!(line, &character.rows, smush_data) {
            smush.combine(shift, buffer_row, char_row, self.config.direction);
        }
        true
    }

    fn render_line_full_width<'a>(&self, mut string: &'a str) -> (Vec<Vec<u8>>, usize, &'a str) {
        let mut line: Vec<Vec<u8>> = vec![Vec::new(); self.font.header().height.get()];
        let mut width = 0;
        let mut chars = string.chars();
        let mut before_space = None;
        let mut saved = false;
        let mut overfull = false;
        while let Some(c) = chars.next() {
            let c = if c == '\t' { ' ' } else { c };
            if c == ' ' && !saved {
                before_space = Some((line.clone(), width, string));
                saved = true;
            }
            if LINE_BREAKS.contains(&c) {
                string = chars.as_str();
                break;
            }
            let Some(character) = self.font.get(c) else {
                continue;
            };
            if self
                .width
                .as_option()
                .is_some_and(|max_width| character.width + width > max_width)
            {
                overfull = true;
                break;
            }
            width += character.width;
            for (buf_row, char_row) in line.iter_mut().zip(&character.rows) {
                buf_row.extend(char_row.bidi_chars(self.config.direction));
            }

            string = chars.as_str();
            if c != ' ' {
                saved = false;
            }
        }
        if overfull {
            if let Some(saved) = before_space {
                (line, width, string) = saved;
                string = string.trim_start_matches([' ', '\t']);
            }
        }
        (line, width, string)
    }

    fn row_smush_data(&self, buffer: &[Vec<u8>], char_rows: &[Vec<u8>]) -> Vec<RowSmush> {
        buffer
            .iter()
            .zip(char_rows)
            .map(|(end, start)| {
                let end = RowSmush::count_blanks(end.iter().rev().copied());
                let start = RowSmush::count_blanks(start.bidi_chars(self.config.direction));
                RowSmush::new(end, start, self.config, self.font.header().hardblank)
            })
            .collect()
    }

    fn join(&self, rows: Vec<Vec<u8>>) -> Vec<u8> {
        let mut buffer = Vec::new();
        let mut first = true;
        for row in rows {
            if !first {
                buffer.push(b'\n');
            }
            if self.config.direction == PrintDirection::LeftToRight {
                buffer.extend(row);
            } else {
                buffer.extend(row.into_iter().rev());
            }
            first = false;
        }
        buffer
    }

    fn stack(&self, lines: Vec<Vec<Vec<u8>>>, width: usize) -> Vec<Vec<u8>> {
        let mut rows = Vec::new();
        if self.config.full_height() {
            rows.extend(lines.into_iter().flatten());
        } else {
            for line in lines {
                debug_assert!(
                    line.iter().all(|row| row.len() >= width),
                    "insufficient padding"
                );
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
        if !self.config.full_height() {
            while rows
                .last()
                .is_some_and(|row| row.iter().all(|&c| c == b' '))
            {
                drop(rows.pop());
            }
        }
        rows
    }

    fn column_smush_data(
        &self,
        end: &[Vec<u8>],
        start: &[Vec<u8>],
        width: usize,
    ) -> Vec<ColumnSmush> {
        (0..width)
            .map(|i| {
                let end_counts = BlanksAndBars::count(end.iter().rev().map(|row| row[i]));
                let start_counts = BlanksAndBars::count(start.iter().map(|row| row[i]));
                ColumnSmush::new(&end_counts, &start_counts, self.config.vertical_layout)
            })
            .collect()
    }

    fn trimming(line: impl IntoIterator<Item = impl IntoIterator<Item = u8>>) -> usize {
        line.into_iter()
            .map(|row| row.into_iter().take_while(|&c| c == b' ').count())
            .min()
            .unwrap_or(0)
    }
}

impl<'font> Renderer<'font, Unbounded> {
    /// Creates a new renderer. The default alignment is [`Alignment::Start`]; the rest of the
    /// settings are taken from the font.
    pub const fn new(font: &'font Font) -> Self {
        let config = Config::from_header(font.header());
        Self {
            font,
            config,
            width: Unbounded,
        }
    }

    /// Set a maximum width (in bytes) for the output. This changes the output type to [`Option`]
    /// since there might be characters that cannot fit in the specified width. The recommended
    /// minimum for `width` is [`font.max_width()`](Font::max_width). For a smaller width, the
    /// output may still be `Some`, depending on the input string being rendered.
    pub const fn max_width(self, width: usize) -> Renderer<'font, Bounded> {
        Renderer {
            font: self.font,
            config: self.config,
            width: Bounded(width),
        }
    }

    #[must_use]
    fn render_bytes(&self, mut string: &str) -> Vec<u8> {
        let mut lines: Vec<Vec<Vec<u8>>> = Vec::new();
        let mut width = 0;
        while !string.is_empty() {
            let (line, line_width, rest) = if self.config.full_width() {
                self.render_line_full_width(string)
            } else {
                self.render_line(string)
            };
            lines.push(line);
            width = width.max(line_width);
            string = rest;
        }
        for row in lines.iter_mut().flatten() {
            for c in row.iter_mut() {
                if self.font.header().hardblank == *c {
                    *c = b' ';
                }
            }
            *row = self.config.alignment.pad(mem::take(row), width);
        }
        let rows = self.stack(lines, width);
        self.join(rows)
    }
}

impl Renderer<'_, Bounded> {
    #[must_use]
    fn render_bytes(&self, mut string: &str) -> Option<Vec<u8>> {
        let mut lines: Vec<Vec<Vec<u8>>> = Vec::new();
        let mut width = if self.config.alignment == Alignment::Start {
            0
        } else {
            self.width.0
        };
        while !string.is_empty() {
            let (line, line_width, rest) = if self.config.full_width() {
                self.render_line_full_width(string)
            } else {
                self.render_line(string)
            };
            if rest == string {
                return None;
            }
            lines.push(line);
            debug_assert!(line_width <= self.width.0, "rendered line too wide");
            width = width.max(line_width);
            string = rest;
        }
        for row in lines.iter_mut().flatten() {
            for c in row.iter_mut() {
                if self.font.header().hardblank == *c {
                    *c = b' ';
                }
            }
            *row = self.config.alignment.pad(mem::take(row), width);
        }
        let rows = self.stack(lines, width);
        Some(self.join(rows))
    }
}

/// Trait to generically bound the renderer output
///
/// Implementations are provided for [`Vec<u8>`] and [`String`]. Effectively a version of
/// [`From<Vec<u8>>`].
pub trait RenderOutput {
    /// Convert the byte level rendered output.
    fn from_bytes(bytes: Vec<u8>) -> Self;
}

impl RenderOutput for Vec<u8> {
    fn from_bytes(bytes: Vec<u8>) -> Self {
        bytes
    }
}

impl RenderOutput for String {
    fn from_bytes(bytes: Vec<u8>) -> Self {
        Self::from_utf8_lossy(&bytes).into_owned()
    }
}

/// Trait for setting the width in a type-safe manner
///
/// Currently only implemented for the newtypes [`Bounded`] and [`Unbounded`], but could in
/// principle also be implemented for a type like [`Option<usize>`].
pub trait WidthConfig: Sized {
    /// Generic wrapper for the output type. Needed because bounding the width makes the rendering
    /// fallible.
    type Output<O>;

    /// Run the rendering algorithm.
    fn render_bytes(renderer: Renderer<'_, Self>, string: &str) -> Self::Output<Vec<u8>>;
    /// Convert the wrapped output to the generically specified type.
    fn map<O: RenderOutput>(bytes: Self::Output<Vec<u8>>) -> Self::Output<O>;
    /// `Some(width)` on [`Bounded`], `None` on [`Unbounded`], used dynamically by the rendering
    /// algorithm.
    fn as_option(&self) -> Option<usize>;
}

/// Statically denotes that the output width is unbounded
#[derive(Debug, Clone, Copy)]
pub struct Unbounded;

impl WidthConfig for Unbounded {
    type Output<O> = O;

    fn render_bytes(renderer: Renderer<'_, Self>, string: &str) -> Vec<u8> {
        renderer.render_bytes(string)
    }

    fn map<O: RenderOutput>(bytes: Vec<u8>) -> O {
        O::from_bytes(bytes)
    }

    fn as_option(&self) -> Option<usize> {
        None
    }
}

/// Statically denotes that the output width is bounded
///
/// See [`Renderer::max_width`] and [`Renderer::render`] for the effect on output and output types.
#[derive(Debug, Clone, Copy)]
pub struct Bounded(usize);

impl WidthConfig for Bounded {
    type Output<O> = Option<O>;

    fn render_bytes(renderer: Renderer<'_, Self>, string: &str) -> Option<Vec<u8>> {
        renderer.render_bytes(string)
    }

    fn map<O: RenderOutput>(bytes: Option<Vec<u8>>) -> Option<O> {
        bytes.map(O::from_bytes)
    }

    fn as_option(&self) -> Option<usize> {
        Some(self.0)
    }
}

/// The choice of line alignment for multi-line output
///
/// The alignment is defined relative to the printing direction, so [`Alignment::Start`] will align
/// text on the left if printing left-to-right and on the right if printing right-to-left (and the
/// opposite for [`Alignment::End`]).
///
/// The alignment is only relevant if a maximum width is set or when rendering text with line
/// breaking characters (newline, carriage return, vertical tab, and form feed).
///
/// The default is [`Alignment::Start`].
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Alignment {
    /// Left align if rendering [left-to-right](PrintDirection::LeftToRight), right align if
    /// rendering [right-to-left](PrintDirection::RightToLeft). This is the default.
    #[default]
    Start,
    /// Center align. This is still affected by [`PrintDirection`]: if the final width and the width
    /// required to render a given line have different parity (ie one is odd and the other is even),
    /// the extra padding is rounded so that there's one fewer blank towards the *start*.
    Center,
    /// Right align if rendering [left-to-right](PrintDirection::LeftToRight), left align if
    /// rendering [right-to-left](PrintDirection::RightToLeft).
    End,
}

impl Alignment {
    fn pad(self, mut row: Vec<u8>, to_width: usize) -> Vec<u8> {
        let Some(padding) = to_width.checked_sub(row.len()) else {
            return row;
        };
        match self {
            Self::Start => {
                row.extend(repeat_n(b' ', padding));
                row
            }
            Self::Center => {
                let start = padding / 2;
                repeat_n(b' ', start)
                    .chain(row)
                    .chain(repeat_n(b' ', padding - start))
                    .collect()
            }
            Self::End => repeat_n(b' ', padding).chain(row).collect(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Config {
    horizontal_layout: HorizontalLayout,
    vertical_layout: VerticalLayout,
    direction: PrintDirection,
    alignment: Alignment,
}

impl Config {
    const fn from_header(header: &Header) -> Self {
        Self {
            horizontal_layout: header.horizontal_layout,
            vertical_layout: header.vertical_layout,
            direction: header.print_direction,
            alignment: Alignment::Start,
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
    Keep {
        end_blanks: usize,
    },
    Overwrite {
        start_blanks: usize,
    },
    Smush {
        end_blanks: usize,
        start_blanks: usize,
        smush: Option<u8>,
    },
}

impl RowSmush {
    fn new(
        end: (usize, Option<u8>),
        start: (usize, Option<u8>),
        config: Config,
        hardblank: Hardblank,
    ) -> Self {
        match (end.1, start.1) {
            (_, None) => Self::Keep { end_blanks: end.0 },
            (None, _) => Self::Overwrite {
                start_blanks: start.0,
            },
            (Some(end_char), Some(start_char)) => {
                let smush = config
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
        buffer_row: &mut Vec<u8>,
        char_row: &[u8],
        direction: PrintDirection,
    ) {
        match self {
            Self::Keep { .. } => {
                if char_row.len() <= shift {
                    buffer_row.truncate(buffer_row.len() + char_row.len() - shift);
                } else {
                    buffer_row.extend(repeat_n(b' ', char_row.len() - shift));
                }
            }
            Self::Overwrite { .. } => {
                let skip = shift.saturating_sub(buffer_row.len());
                buffer_row.truncate(buffer_row.len().saturating_sub(shift));
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

    fn count_blanks(chars: impl Iterator<Item = u8>) -> (usize, Option<u8>) {
        let mut blanks = 0;
        let mut next = None;
        for c in chars {
            if c == b' ' {
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
        smush: Option<u8>,
    },
    EndBar {
        end_blanks: usize,
        start_blanks: usize,
        start_offset: usize,
        smush: Option<u8>,
    },
    StartBar {
        smush: Option<u8>,
        end_offset: usize,
        end_blanks: usize,
        start_blanks: usize,
    },
    DoubleSmush {
        end_blanks: usize,
        bars: usize,
        start_blanks: usize,
        smush: Option<(u8, u8)>,
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
                        smush: end.next.and_then(|end| layout.smush(end, b'|')),
                        end_offset: end.total(),
                        end_blanks: end.blanks,
                        start_blanks: start.blanks,
                    },
                    Ordering::Equal => {
                        let smush = end
                            .next
                            .and_then(|end| layout.smush(end, b'|'))
                            .zip(start.next.and_then(|start| layout.smush(b'|', start)));
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
                        smush: start.next.and_then(|start| layout.smush(b'|', start)),
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

    fn combine(&self, rows: &mut [Vec<u8>], line: &[Vec<u8>], i: usize, shift: usize) {
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
    next: Option<u8>,
}

impl BlanksAndBars {
    fn count(chars: impl Iterator<Item = u8>) -> Self {
        let mut blanks = 0;
        let mut bars = 0;
        let mut next = None;
        let mut in_bars = false;
        for c in chars {
            match (c, in_bars) {
                (b' ', false) => blanks += 1,
                (b'|', false) => {
                    bars += 1;
                    in_bars = true;
                }
                (b'|', true) => bars += 1,
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

    const fn next_to_blank(&self) -> Option<u8> {
        if self.bars > 0 { Some(b'|') } else { self.next }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use crate::font::Font;

    use super::{Alignment, PrintDirection, Renderer};

    pub(crate) use super::layout::test::{check_horizontal_standard, check_vertical_standard};

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
    fn hello_world_flipped() {
        let rendered: String = Renderer::new(&Font::standard())
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
        let rendered: String = Renderer::new(&Font::standard())
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
        let rendered: String = Renderer::new(&Font::standard())
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
        let rendered: String = Renderer::new(&Font::standard())
            .max_width(80)
            .render("floccinaucinihilipilification")
            .unwrap();

        let expected = r"  __ _                _                        _       _ _     _ _ _       _ _ 
 / _| | ___   ___ ___(_)_ __   __ _ _   _  ___(_)_ __ (_) |__ (_) (_)_ __ (_) |
| |_| |/ _ \ / __/ __| | '_ \ / _` | | | |/ __| | '_ \| | '_ \| | | | '_ \| | |
|  _| | (_) | (_| (__| | | | | (_| | |_| | (__| | | | | | | | | | | | |_) | | |
|_| |_|\___/ \___\___|_|_| |_|\__,_|\__,_|\___|_|_| |_|_|_| |_|_|_|_| .__/|_|_|
(_)/ _(_) ___ __ _| |_(_) ___  _ __                                 |_|        
| | |_| |/ __/ _` | __| |/ _ \| '_ \                                           
| |  _| | (_| (_| | |_| | (_) | | | |                                          
|_|_| |_|\___\__,_|\__|_|\___/|_| |_|                                          ";
        assert_eq!(rendered, expected);
    }

    #[test]
    fn forced_line_break_lots_of_spaces() {
        let rendered: String = Renderer::new(&Font::standard())
            .max_width(80)
            .render("floccinauci                  nihilipilification")
            .unwrap();
        let expected = r"  __ _                _                        _                   
 / _| | ___   ___ ___(_)_ __   __ _ _   _  ___(_)                  
| |_| |/ _ \ / __/ __| | '_ \ / _` | | | |/ __| |                  
|  _| | (_) | (_| (__| | | | | (_| | |_| | (__| |                  
|_| |_|\___/ \___\___|_|_| |_|\__,_|\__,_|\___|_|_   _             
 _ __ (_) |__ (_) (_)_ __ (_) (_)/ _(_) ___ __ _| |_(_) ___  _ __  
| '_ \| | '_ \| | | | '_ \| | | | |_| |/ __/ _` | __| |/ _ \| '_ \ 
| | | | | | | | | | | |_) | | | |  _| | (_| (_| | |_| | (_) | | | |
|_| |_|_|_| |_|_|_|_| .__/|_|_|_|_| |_|\___\__,_|\__|_|\___/|_| |_|
                    |_|                                            ";
        assert_eq!(rendered, expected);
    }

    #[cfg(feature = "fonts")]
    #[test]
    fn mini_pangram() {
        use letrs_fonts::FontFile;

        let font_mini = Font::built_in(FontFile::Mini);
        let rendered: String = Renderer::new(&font_mini)
            .max_width(80)
            .render("The quick brown fox jumps over the lazy dog.")
            .unwrap();
        let expected = r"___                                   _                                
 ||_  _   _.   o _|  |_ .__     ._  _|__     o   ._ _ ._  _  _    _ ._ 
 || |(/_ (_||_||(_|< |_)|(_)\/\/| |  |(_)><  ||_|| | ||_)_> (_)\/(/_|  
           |                                _|        |                
                                                                       
_|_|_  _  | _._     _| _  _                                            
 |_| |(/_ |(_|/_\/ (_|(_)(_|o                                          
                /         _|                                           ";
        assert_eq!(rendered, expected);
    }
}
