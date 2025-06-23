use std::collections::HashMap;
use std::num::ParseIntError;

use itertools::Itertools as _;
use thiserror::Error;

use crate::render::{HorizontalLayout, LayoutParseError, PrintDirection, Renderer, VerticalLayout};
use crate::str_ext::StrExt as _;

pub(crate) const DEFAULT_CODEPOINTS: [u32; 102] = [
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
    122, 123, 124, 125, 126, 196, 214, 220, 228, 246, 252, 223,
];

#[derive(Debug)]
pub struct Font {
    header: Header,
    comments: String,
    characters: HashMap<u32, Character>,
    code_tagged_characters: HashMap<u32, String>,
    ignored_codepoints: HashMap<u32, String>,
}

impl Font {
    pub(crate) const STANDARD: &'static str = include_str!("standard.flf");

    pub fn parse(font: &str) -> Result<Self, FontError> {
        Self::parse_strict(font).map(|(font, _)| font)
    }

    pub fn parse_strict(font_string: &str) -> Result<(Self, Vec<FontWarning>), FontError> {
        let mut warnings = Vec::new();
        if !font_string.is_ascii() {
            warnings.push(FontWarning::NonAscii);
        }
        let font_string = font_string.replace("\r\n", "\n").replace('\r', "\n");
        let mut lines = font_string.lines();
        let Some(header_line) = lines.next() else {
            return Err(FontError::BadHeader(HeaderError::Missing));
        };
        let header = Header::parse(header_line, &mut warnings)?;
        let comments = lines.by_ref().take(header.comment_lines).join("\n");
        let mut font = Self {
            header,
            comments,
            characters: HashMap::new(),
            code_tagged_characters: HashMap::new(),
            ignored_codepoints: HashMap::new(),
        };
        font.parse_characters(lines, &mut warnings)?;
        Ok((font, warnings))
    }

    #[must_use]
    pub fn render(&self, string: &str) -> String {
        Renderer::new(self).render_unbounded(string)
    }

    pub(crate) fn parse_characters<'a>(
        &mut self,
        mut lines: impl Iterator<Item = &'a str>,
        warnings: &mut Vec<FontWarning>,
    ) -> Result<(), FontError> {
        let default_char_chunks = lines
            .by_ref()
            .take(DEFAULT_CODEPOINTS.len() * self.header.height)
            .chunks(self.header.height);
        for (codepoint, rows) in DEFAULT_CODEPOINTS
            .into_iter()
            .zip(default_char_chunks.into_iter())
        {
            let character = Character::parse(rows, codepoint, &self.header, warnings)?;
            drop(self.characters.insert(codepoint, character));
        }
        if self.characters.len() != DEFAULT_CODEPOINTS.len() {
            return Err(FontError::MissingDefaultCharacters(self.characters.len()));
        }
        let mut processed_chars = 0;
        for mut rows in &lines.by_ref().chunks(self.header.height + 1) {
            let line = rows.next().expect("chunk size >= 1");
            let (codepoint, desc) = line
                .split_once(' ')
                .map_or((line, None), |(codepoint, desc)| {
                    (codepoint, Some(desc.trim()))
                });
            let (codepoint, positive) = Self::parse_codepoint(codepoint)?;
            if positive {
                if let Some(desc) = desc {
                    drop(
                        self.code_tagged_characters
                            .insert(codepoint, desc.to_owned()),
                    );
                }
                let character = Character::parse(rows, codepoint, &self.header, warnings)?;
                drop(self.characters.insert(codepoint, character));
            } else {
                drop(self.ignored_codepoints.insert(codepoint, rows.join("\n")));
            }
            processed_chars += 1;
        }
        if processed_chars < self.header.codetag_count {
            warnings.push(FontWarning::TooFewCodetags {
                found: processed_chars,
                expected: self.header.codetag_count,
            });
        }
        Ok(())
    }

    pub(crate) fn parse_codepoint(codepoint: &str) -> Result<(u32, bool), FontError> {
        let (positive, codepoint) = codepoint
            .strip_prefix('-')
            .map_or((true, codepoint), |codepoint| (false, codepoint));
        let codepoint = if let Some(codepoint) = codepoint.strip_prefix("0x") {
            u32::from_str_radix(codepoint, 16)
        } else if let Some(codepoint) = codepoint.strip_prefix("0") {
            u32::from_str_radix(codepoint, 8)
        } else {
            codepoint.parse()
        };
        let codepoint = codepoint.map_err(FontError::InvalidCodePoint)?;
        if (positive && codepoint <= 0x7FFF_FFFF) || (!positive && codepoint <= 0x8000_0000) {
            Ok((codepoint, positive))
        } else {
            Err(FontError::CodePointOutOfRange(codepoint))
        }
    }

    #[must_use]
    pub fn comments(&self) -> &str {
        &self.comments
    }

    #[must_use]
    pub const fn header(&self) -> &Header {
        &self.header
    }

    #[must_use]
    pub fn standard() -> Self {
        Self::parse(Self::STANDARD).expect("Should be tested")
    }

    pub(crate) fn get(&self, char: char) -> Option<&Character> {
        self.characters
            .get(&u32::from(char))
            .or_else(|| self.characters.get(&0))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Header {
    pub hardblank: Hardblank,
    pub height: usize,
    pub baseline: usize,
    pub max_length: usize,
    pub comment_lines: usize,
    pub horizontal_layout: HorizontalLayout,
    pub vertical_layout: VerticalLayout,
    pub print_direction: PrintDirection,
    pub codetag_count: usize,
}

impl Header {
    pub(crate) fn parse(
        header_line: &str,
        warnings: &mut Vec<FontWarning>,
    ) -> Result<Self, HeaderError> {
        let mut parameters = header_line
            .split(' ')
            .filter(|parameter| !parameter.is_empty());
        let Some(
            [
                signature_and_hardblank,
                height,
                baseline,
                max_length,
                old_layout,
                comment_lines,
            ],
        ) = parameters.next_array()
        else {
            return Err(HeaderError::NotEnoughParameters(header_line.to_owned()));
        };
        let print_direction = parameters.next();
        let full_layout = parameters.next();
        let codetag_count = parameters.next();

        let Some(hardblank) = signature_and_hardblank.strip_prefix("flf2a") else {
            return Err(HeaderError::UnknownSignature(
                signature_and_hardblank.to_owned(),
            ));
        };
        let Ok(hardblank) = hardblank.chars().exactly_one() else {
            return Err(HeaderError::Hardblank(hardblank.to_owned()));
        };
        let hardblank = hardblank
            .try_into()
            .map_err(HeaderError::InvalidHardblankChar)?;
        let height = height.parse()?;
        if height == 0 {
            return Err(HeaderError::ZeroHeight);
        }
        let baseline = baseline.parse().unwrap_or_else(|_| {
            warnings.push(FontWarning::Baseline(baseline.to_owned()));
            1
        });
        if !(0 < baseline && baseline <= height) {
            warnings.push(FontWarning::BaselineOutOfRange { baseline, height });
        }
        let max_length = max_length.parse()?;
        let comment_lines = comment_lines.parse()?;
        let old_layout = old_layout.parse()?;
        let full_layout = full_layout.map(str::parse).transpose()?;
        let horizontal_layout = HorizontalLayout::parse(old_layout, full_layout)?;
        let vertical_layout = VerticalLayout::parse(full_layout)?;
        let print_direction = PrintDirection::parse(print_direction)?;
        let codetag_count = codetag_count.map(str::parse).transpose()?.unwrap_or(0);
        let header = Self {
            hardblank,
            height,
            baseline,
            max_length,
            comment_lines,
            horizontal_layout,
            vertical_layout,
            print_direction,
            codetag_count,
        };
        Ok(header)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Hardblank(char);

impl PartialEq<char> for Hardblank {
    fn eq(&self, other: &char) -> bool {
        self.0 == *other
    }
}

impl TryFrom<char> for Hardblank {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        if matches!(value, ' ' | '\r' | '\n' | '\0') {
            Err(value)
        } else {
            Ok(Self(value))
        }
    }
}

#[derive(Debug)]
pub(crate) struct Character {
    pub width: usize,
    pub rows: Vec<String>,
}

impl Character {
    pub(crate) fn parse<'a>(
        rows: impl Iterator<Item = &'a str>,
        codepoint: u32,
        header: &Header,
        warnings: &mut Vec<FontWarning>,
    ) -> Result<Self, FontError> {
        let rows = rows
            .map(|line| {
                let last = line.last()?;
                Some(line.trim_end_matches(last).to_owned())
            })
            .collect::<Option<Vec<String>>>()
            .ok_or(FontError::EmptyRow(codepoint))?;
        let width = match rows.iter().map(String::len).unique().exactly_one() {
            Ok(width) => width,
            Err(widths) => {
                warnings.push(FontWarning::InconsistentWidth(codepoint));
                widths.max().expect("height is non-zero")
            }
        };
        if width > header.max_length {
            warnings.push(FontWarning::ExcessLength {
                codepoint,
                width,
                max_length: header.max_length,
            });
        }
        Ok(Self { width, rows })
    }
}

#[derive(Debug, Error)]
pub enum FontError {
    #[error("Bad header: {0}")]
    BadHeader(#[from] HeaderError),
    #[error("Not enough required FIGcharacters, found {0}, expected 102")]
    MissingDefaultCharacters(usize),
    #[error("{0}")]
    InvalidCodePoint(ParseIntError),
    #[error("{0}")]
    CodePointOutOfRange(u32),
    #[error("empty row in FIGcharacter {0}")]
    EmptyRow(u32),
}

#[derive(Debug, Error)]
pub enum HeaderError {
    #[error("missing header")]
    Missing,
    #[error(r#""{0}" does not include enough parameters"#)]
    NotEnoughParameters(String),
    #[error(r#"{0} does not begin with "flf2a""#)]
    UnknownSignature(String),
    #[error(r#"hardblank "{0}" is not exactly one character"#)]
    Hardblank(String),
    #[error("'{0}' must not be the hardblank")]
    InvalidHardblankChar(char),
    #[error("{0}")]
    ParseInt(#[from] ParseIntError),
    #[error(r#""{0}" is an invalid print direction, expecting 0 or 1"#)]
    PrintDirection(String),
    #[error("{0}")]
    Layout(#[from] LayoutParseError),
    #[error("height parameter is 0")]
    ZeroHeight,
}

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum FontWarning {
    #[error("font contains non-ascii characters")]
    NonAscii,
    #[error(r#"could not parse "{0}" as the baseline parameter"#)]
    Baseline(String),
    #[error("baseline {baseline} not between 1 and {height} (height)")]
    BaselineOutOfRange { baseline: usize, height: usize },
    #[error("found {found} codetags but expected {expected} from header")]
    TooFewCodetags { found: usize, expected: usize },
    #[error("FIGcharacter with codepoint {} has inconsistent width", Self::char_debug(*.0))]
    InconsistentWidth(u32),
    #[error("FIGcharacter with codepoint {} has width {width} > {max_length} (from header)", Self::char_debug(*.codepoint))]
    ExcessLength {
        codepoint: u32,
        width: usize,
        max_length: usize,
    },
}

impl FontWarning {
    pub(crate) fn char_debug(codepoint: u32) -> String {
        char::try_from(codepoint).map_or_else(
            |_| format!("\\u{{{codepoint:04X}}}"),
            |char| char.to_string(),
        )
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::Font;

    #[test]
    fn parse_standard() {
        use crate::render::test::{check_horizontal_standard, check_vertical_standard};

        let (font, warnings) = Font::parse_strict(Font::STANDARD).unwrap();
        assert!(warnings.is_empty());
        assert_eq!(font.header.hardblank, '$');
        assert_eq!(font.header.height, 6);
        assert_eq!(font.header.baseline, 5);
        assert_eq!(font.header.max_length, 16);

        check_horizontal_standard(font.header.horizontal_layout);
        check_vertical_standard(font.header.vertical_layout);
    }
}
