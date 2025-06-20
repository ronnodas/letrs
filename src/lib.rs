pub mod layout;

use std::collections::HashMap;
use std::num::ParseIntError;

use itertools::Itertools;
use thiserror::Error;

use layout::{HorizontalLayout, LayoutParseError, VerticalLayout};

const DEFAULT_CODEPOINTS: [u32; 102] = [
    32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55,
    56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
    80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
    122, 123, 124, 125, 126, 196, 214, 220, 228, 246, 252, 223,
];

pub struct Font {
    header: Header,
    comments: String,
    characters: HashMap<u32, Character>,
    code_tagged_characters: HashMap<u32, String>,
    ignored_codepoints: HashMap<u32, String>,
}

impl Font {
    const STANDARD: &'static str = include_str!("standard.flf");

    pub fn parse(font: &str) -> Result<Self, Error> {
        Self::parse_strict(font).map(|(font, _)| font)
    }

    pub fn parse_strict(font_string: &str) -> Result<(Self, Vec<Warning>), Error> {
        let mut warnings = Vec::new();
        if !font_string.is_ascii() {
            warnings.push(Warning::NonAscii);
        }
        let font_string = font_string.replace("\r\n", "\n").replace("\r", "\n");
        let mut lines = font_string.lines();
        let Some(header_line) = lines.next() else {
            return Err(Error::BadHeader(HeaderError::Missing));
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

    fn parse_characters<'a>(
        &mut self,
        mut lines: impl Iterator<Item = &'a str>,
        warnings: &mut Vec<Warning>,
    ) -> Result<(), Error> {
        for (codepoint, lines) in DEFAULT_CODEPOINTS
            .into_iter()
            .zip(lines.by_ref().chunks(self.header.height).into_iter())
        {
            let character = Character::parse(lines, &self.header, warnings)?;
            _ = self.characters.insert(codepoint, character);
        }
        if self.characters.len() != DEFAULT_CODEPOINTS.len() {
            return Err(Error::MissingDefaultCharacters(self.characters.len()));
        }
        for mut lines in lines.by_ref().chunks(self.header.height + 1).into_iter() {
            let line = lines.next().expect("chunk size >= 1");
            let (codepoint, desc) = line
                .split_once(' ')
                .map_or((line, None), |(codepoint, desc)| {
                    (codepoint, Some(desc.trim()))
                });
            let (codepoint, positive) = Self::parse_codepoint(codepoint)?;
            if positive {
                if let Some(desc) = desc {
                    self.code_tagged_characters
                        .insert(codepoint, desc.to_owned());
                }
                let character = Character::parse(lines, &self.header, warnings)?;
                self.characters.insert(codepoint, character);
            } else {
                self.ignored_codepoints.insert(codepoint, lines.join("\n"));
            }
        }
        if self.characters.len() + self.ignored_codepoints.len() < 102 + self.header.codetag_count {
            warnings.push(Warning::TooFewCodetags {
                found: self.characters.len() + self.ignored_codepoints.len() - 102,
                expected: self.header.codetag_count,
            });
        }
        Ok(())
    }

    fn parse_codepoint(codepoint: &str) -> Result<(u32, bool), Error> {
        todo!()
    }

    pub fn comments(&self) -> &str {
        &self.comments
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn standard() -> Self {
        Self::parse(Self::STANDARD).expect("Should be tested")
    }
}

#[derive(Clone, Copy)]
pub struct Header {
    pub hardblank: Hardblank,
    pub height: usize,
    pub baseline: usize,
    pub max_length: usize,
    pub comment_lines: usize,
    pub horizontal_layout: layout::HorizontalLayout,
    pub vertical_layout: layout::VerticalLayout,
    pub print_direction: PrintDirection,
    pub codetag_count: usize,
}

impl Header {
    fn parse(header_line: &str, warnings: &mut Vec<Warning>) -> Result<Self, HeaderError> {
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
        let hardblank = hardblank.try_into()?;
        let height = height.parse()?;
        let baseline = match baseline.parse() {
            Ok(baseline) => baseline,
            Err(_) => {
                warnings.push(Warning::Baseline(baseline.to_owned()));
                1
            }
        };
        if !(0 < baseline && baseline <= height) {
            warnings.push(Warning::BaselineOutOfRange(baseline))
        }
        let max_length = max_length.parse()?;
        let comment_lines = comment_lines.parse()?;
        let old_layout = old_layout.parse()?;
        let full_layout = full_layout.map(|x| x.parse()).transpose()?;
        let horizontal_layout = HorizontalLayout::parse(old_layout, full_layout)?;
        let vertical_layout = VerticalLayout::parse(full_layout)?;
        let print_direction = match print_direction {
            None | Some("0") => PrintDirection::LeftToRight,
            Some("1") => PrintDirection::RightToLeft,
            Some(other) => return Err(HeaderError::PrintDirection(other.to_owned())),
        };
        let codetag_count = codetag_count.map(|x| x.parse()).transpose()?.unwrap_or(0);
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

#[derive(Clone, Copy)]
pub struct Hardblank(char);

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

struct Character {}
impl Character {
    fn parse<'a>(
        lines: impl Iterator<Item = &'a str>,
        header: &Header,
        warnings: &mut Vec<Warning>,
    ) -> Result<Self, Error> {
        todo!()
    }
}

#[derive(Clone, Copy)]
pub enum PrintDirection {
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Bad header: {0}")]
    BadHeader(#[from] HeaderError),
    #[error("Not enough required FIGcharacters, found {0}, expected 102")]
    MissingDefaultCharacters(usize),
}

#[derive(Debug, Error)]
pub enum HeaderError {
    #[error("Missing header")]
    Missing,
    #[error(r#""{0}" does not include enough parameters"#)]
    NotEnoughParameters(String),
    #[error(r#"{0} does not begin with "flf2a""#)]
    UnknownSignature(String),
    #[error(r#"hardblank "{0}" should be exactly one character"#)]
    Hardblank(String),
    #[error("'{0}' may not be the hardblank")]
    InvalidHardblankChar(char),
    #[error("{0}")]
    ParseInt(#[from] ParseIntError),
    #[error(r#""{0}" is an invalid print direction, expecting 0 or 1"#)]
    PrintDirection(String),
    #[error("{0}")]
    Layout(#[from] LayoutParseError),
}

impl From<char> for HeaderError {
    fn from(value: char) -> Self {
        Self::InvalidHardblankChar(value)
    }
}

pub enum Warning {
    NonAscii,
    Baseline(String),
    BaselineOutOfRange(usize),
    TooFewCodetags { found: usize, expected: usize },
}

#[cfg(test)]
mod tests {
    use crate::Font;

    #[test]
    fn null_char() {
        assert_eq!(u32::from('\0'), 0)
    }

    #[test]
    fn parse_standard() {
        let (_font, warnings) = Font::parse_strict(Font::STANDARD).unwrap();
        assert!(warnings.is_empty())
    }
}
