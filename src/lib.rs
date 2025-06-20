use std::collections::HashMap;
use std::num::ParseIntError;

use itertools::Itertools;
use thiserror::Error;

pub struct Font {
    header: Header,
    comments: String,
    characters: HashMap<u32, Character>,
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
        let (header, header_warnings) = Header::parse(header_line)?;
        warnings.extend(header_warnings);
        todo!()
    }
}

struct Header {
    hardblank: Hardblank,
    height: usize,
    baseline: usize,
    max_length: usize,
    comment_lines: usize,
    horizontal_layout: HorizontalLayout,
    vertical_layout: VerticalLayout,
    print_direction: PrintDirection,
    codetag_count: usize,
}

impl Header {
    fn parse(header_line: &str) -> Result<(Self, Vec<Warning>), HeaderError> {
        let mut warnings = Vec::new();
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
        let full_layout = parameters.next();
        let (horizontal_layout, vertical_layout) = Self::parse_layouts(old_layout, full_layout)?;
        let print_direction = match parameters.next() {
            None | Some("0") => PrintDirection::LeftToRight,
            Some("1") => PrintDirection::RightToLeft,
            Some(other) => return Err(HeaderError::PrintDirection(other.to_owned())),
        };
        let codetag_count = match parameters.next() {
            Some(count) => count.parse()?,
            None => 0,
        };
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
        Ok((header, warnings))
    }

    fn parse_layouts(
        old_layout: &str,
        full_layout: Option<&str>,
    ) -> Result<(HorizontalLayout, VerticalLayout), HeaderError> {
        todo!()
    }
}

struct Hardblank(char);

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

enum HorizontalLayout {}

enum VerticalLayout {}

enum PrintDirection {
    LeftToRight,
    RightToLeft,
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Bad header: {0}")]
    BadHeader(#[from] HeaderError),
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
