//! FIGfonts
//!
//! Font types and the logic for parsing `.flf` files.

mod header;

use std::collections::HashMap;
use std::num::NonZero;
use std::str;

use bstr::{BString, ByteSlice as _};
use itertools::Itertools as _;
use thiserror::Error;

use crate::render::{Renderer, Unbounded};

#[cfg(feature = "fonts")]
pub use letrs_fonts::FontFile;
pub use header::{Hardblank, Header, HeaderError, PrintDirection};

/// The 102 codepoints for characters that are included in all FIGfonts
///
/// Consists of 95 printable ASCII characters and 7 Deutsch characters from the Latin-1 encoding.
pub const DEFAULT_CODEPOINTS: [u8; 102] = *b" !\"#$%&'()*+,-./0123456789:;<=>?@\
                                             ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`\
                                             abcdefghijklmnopqrstuvwxyz{|}~\
                                             \xc4\xd6\xdc\xe4\xf6\xfc\xdf";

/// A FIGfont
#[derive(Debug)]
pub struct Font {
    header: Header,
    comments: String,
    characters: HashMap<u32, Character>,
    code_tagged_characters: HashMap<u32, String>,
    ignored_characters: HashMap<u32, String>,
    max_width: usize,
    is_utf8: bool,
}

impl Font {
    pub(crate) const STANDARD: &'static [u8] = include_bytes!("standard.flf");

    /// Decodes the contents of an `.flf` file.
    ///
    /// If unsure about the input being a fully compliant FIGfont, consider
    /// [`Font::new_with_warnings`]; this method is (currently) a convenience wrapper around
    /// that, ignoring the warnings. Notably, if the font has FIGcharacters that do not use the same
    /// number of sub-characters (bytes) per row, the rendering algorithm may behave unexpectedly,
    /// but this is only emitted as a warning (when using [`Font::new_with_warnings`]) and not
    /// a fatal error.
    ///
    /// # Errors
    /// Returns `Err` on a fatal decoding error; see [`FontError`] for details.
    pub fn new(font: impl AsRef<[u8]>) -> Result<Self, FontError> {
        Self::new_with_warnings(font).map(|(font, _)| font)
    }

    /// Decodes the contents of an `.flf` file and also returns any non-fatal issues found while
    /// decoding.
    ///
    /// See [`FontWarning`] for details on these warnings. Notably, if the font has FIGcharacters
    /// that do not use the same number of sub-characters (bytes) per row, the rendering algorithm
    /// may behave unexpectedly, but this is only emitted as a warning and not a fatal error.
    ///
    /// # Errors
    /// Returns `Err` on a fatal decoding error; see [`FontError`] for details.
    pub fn new_with_warnings(
        bytes: impl AsRef<[u8]>,
    ) -> Result<(Self, Vec<FontWarning>), FontError> {
        let mut warnings = Vec::new();
        let font_string: BString = bytes
            .as_ref()
            .replace("\r\n", "\n")
            .into_iter()
            .map(|c| if c == b'\r' { b'\n' } else { c })
            .collect();

        let mut lines = font_string.lines();
        let Some(header_line) = lines.next() else {
            return Err(FontError::BadHeader(HeaderError::Missing));
        };
        let (header, bad_baseline) = Header::decode_inner(header_line)?;
        if let Some(baseline) = header.baseline {
            let height = header.height;
            if baseline == 0 || baseline > height.get() {
                warnings.push(FontWarning::BaselineOutOfRange { baseline, height });
            }
        } else {
            let bad_baseline = bad_baseline.unwrap_or_else(|| unreachable!());
            warnings.push(FontWarning::Baseline(bad_baseline));
        }
        let comments =
            String::from_utf8_lossy(&bstr::join("\n", lines.by_ref().take(header.comment_lines)))
                .into_owned();
        let mut font = Self {
            header,
            comments,
            characters: HashMap::new(),
            code_tagged_characters: HashMap::new(),
            ignored_characters: HashMap::new(),
            max_width: 0,
            is_utf8: true,
        };
        font.decode_characters(&mut lines, &mut warnings)?;

        if let Some(line) = lines.next() {
            warnings.push(FontWarning::AfterCharacters(line.to_owned().into()));
        }

        Ok((font, warnings))
    }

    /// Decodes the "standard.flf" font, included with this crate.
    #[expect(clippy::missing_panics_doc, reason = "should be caught in tests")]
    #[must_use]
    pub fn standard() -> Self {
        Self::new(Self::STANDARD).expect("Should be tested")
    }

    /// Decodes a FIGfont from the `letrs-fonts` crate.
    ///
    /// Only available with the `fonts` feature.
    #[expect(clippy::missing_panics_doc, reason = "should be caught in tests")]
    #[cfg(feature = "fonts")]
    #[must_use]
    pub fn built_in(font: FontFile) -> Self {
        Self::new(font.as_bytes()).expect("Should be tested")
    }

    /// Renders a string with default settings provided by the font and no `max_width`.
    #[must_use]
    pub fn render(&self, string: &str) -> String {
        Renderer::new(self).render(string)
    }

    /// Renderer using this font and default settings.
    ///
    /// Convenience wrapper around [`Renderer::new`].
    pub const fn renderer(&self) -> Renderer<'_, Unbounded> {
        Renderer::new(self)
    }

    /// The *comments* portion of the FIGfont, between the header and the FIGcharacters. Usually
    /// contains information about the font author.
    #[must_use]
    pub fn comments(&self) -> &str {
        &self.comments
    }

    /// The fully decoded font header.
    #[must_use]
    pub const fn header(&self) -> &Header {
        &self.header
    }

    /// Human-readable information, usually *translation tables*, included in the font as tagged
    /// characters with negative character codes.
    #[must_use]
    pub const fn ignored_characters(&self) -> &HashMap<u32, String> {
        &self.ignored_characters
    }

    /// Returns true if each row of each FIGcharacter in the font is a valid UTF-8 string.
    #[must_use]
    pub const fn is_utf8(&self) -> bool {
        self.is_utf8
    }

    #[expect(
        single_use_lifetimes,
        reason = "https://github.com/rust-lang/rust/issues/137575"
    )]
    pub(crate) fn decode_characters<'a>(
        &mut self,
        mut lines: impl Iterator<Item = &'a [u8]>,
        warnings: &mut Vec<FontWarning>,
    ) -> Result<(), FontError> {
        let default_char_chunks = lines
            .by_ref()
            .take(DEFAULT_CODEPOINTS.len() * self.header.height.get())
            .chunks(self.header.height.get());
        for (codepoint, rows) in DEFAULT_CODEPOINTS
            .into_iter()
            .zip(default_char_chunks.into_iter())
        {
            let character = Character::parse(rows, codepoint.into(), &self.header, warnings)?;
            drop(self.characters.insert(codepoint.into(), character));
        }
        if self.characters.len() != DEFAULT_CODEPOINTS.len() {
            warnings.push(FontWarning::MissingDefaultCharacters(self.characters.len()));
        }
        let mut processed_chars = 0;
        for mut rows in &lines.by_ref().chunks(self.header.height.get() + 1) {
            let line = rows.next().expect("chunk size >= 1");
            let (codepoint, content) = line
                .split_once_str(" ")
                .map_or((line, None), |(codepoint, desc)| {
                    (codepoint, Some(desc.trim_ascii()))
                });
            let (codepoint, positive) =
                Self::parse_codepoint(str::from_utf8(codepoint).map_err(|_| {
                    FontError::InvalidCharacterCode(BString::new(codepoint.to_owned()))
                })?)?;
            if positive {
                if let Some(content) = content {
                    drop(
                        self.code_tagged_characters
                            .insert(codepoint, String::from_utf8_lossy(content).into_owned()),
                    );
                }
                let character = Character::parse(rows, codepoint, &self.header, warnings)?;
                // If two or more FIGcharacters have the same character code, the last one in the
                // FIGfont is the one used. (L1181--1182, figfont.txt)
                drop(self.characters.insert(codepoint, character));
            } else {
                drop(self.ignored_characters.insert(
                    codepoint,
                    String::from_utf8_lossy(&bstr::join(b"\n", rows)).into_owned(),
                ));
            }
            processed_chars += 1;
        }
        if processed_chars < self.header.code_tag_count {
            warnings.push(FontWarning::TooFewCodeTags {
                found: processed_chars,
                expected: self.header.code_tag_count,
            });
        }
        self.max_width = self.characters.values().map(|c| c.width).max().unwrap_or(0);
        self.is_utf8 = self.characters.values().all(|c| c.is_utf8);
        Ok(())
    }

    /// A codepoint in a *code tag* may be positive, denoting an actual character, or negative,
    /// signifying human-readable information, usually translation tables. This is a convenience
    /// function to parse such a codepoint, returning the absolute value and the sign (`true` if
    /// positive, `false` if negative) separately.
    pub(crate) fn parse_codepoint(codepoint: &str) -> Result<(u32, bool), FontError> {
        let (positive, codepoint) = codepoint
            .strip_prefix('-')
            .map_or((true, codepoint), |codepoint| (false, codepoint));
        let result = if codepoint == "0" {
            Ok(0)
        } else if let Some(codepoint) = codepoint.strip_prefix("0x") {
            u32::from_str_radix(codepoint, 16)
        } else if let Some(codepoint) = codepoint.strip_prefix("0X") {
            u32::from_str_radix(codepoint, 16)
        } else if let Some(codepoint) = codepoint.strip_prefix("0") {
            u32::from_str_radix(codepoint, 8)
        } else {
            codepoint.parse()
        };
        let codepoint = result.map_err(|_| FontError::InvalidCharacterCode(codepoint.into()))?;
        if (positive && codepoint <= 0x7FFF_FFFF)
            || (!positive && (2..=0x8000_0000).contains(&codepoint))
        {
            Ok((codepoint, positive))
        } else {
            Err(FontError::CharacterCodeOutOfRange(codepoint))
        }
    }

    pub(crate) fn get(&self, char: char) -> Option<&Character> {
        self.characters
            .get(&u32::from(char))
            .or_else(|| self.characters.get(&0))
    }

    /// The maximum width across all FIGcharacters in this font, measured in sub-characters (bytes).
    #[must_use]
    pub const fn max_width(&self) -> usize {
        self.max_width
    }
}

#[derive(Debug)]
pub(crate) struct Character {
    pub width: usize,
    pub rows: Vec<Vec<u8>>,
    pub is_utf8: bool,
}

impl Character {
    #[expect(
        single_use_lifetimes,
        reason = "https://github.com/rust-lang/rust/issues/137575"
    )]
    pub(crate) fn parse<'a>(
        rows: impl Iterator<Item = &'a [u8]>,
        codepoint: u32,
        header: &Header,
        warnings: &mut Vec<FontWarning>,
    ) -> Result<Self, FontError> {
        let mut too_large_length = None;
        let mut blank_end_mark = false;
        let mut is_utf8 = true;
        let rows = rows
            .map(|line| {
                if line.len() > header.max_length {
                    too_large_length = Some(line.len());
                }
                let &last = line.last()?;
                if last == b' ' {
                    blank_end_mark = true;
                }
                let mark_count = line.iter().rev().take_while(|&&c| c == last).count();
                let (line, _) = line.split_at(line.len() - mark_count);
                if !line.is_utf8() {
                    is_utf8 = false;
                }
                Some(line.to_owned())
            })
            .collect::<Option<Vec<_>>>()
            .ok_or(FontError::EmptyRow(codepoint))?;
        if let Some(length) = too_large_length {
            warnings.push(FontWarning::ExcessLength {
                codepoint,
                length,
                max_length: header.max_length,
            });
        }
        if blank_end_mark {
            warnings.push(FontWarning::BlankEndMark(codepoint));
        }
        let width = match rows.iter().map(Vec::len).unique().exactly_one() {
            Ok(width) => width,
            Err(widths) => {
                warnings.push(FontWarning::InconsistentWidth(codepoint));
                widths.max().expect("height is non-zero")
            }
        };

        Ok(Self {
            width,
            rows,
            is_utf8,
        })
    }
}

/// An error in decoding a FIGfont
#[derive(Debug, Error)]
pub enum FontError {
    /// An error in decoding the header
    #[error("Bad header: {0}")]
    BadHeader(#[from] HeaderError),
    /// An unsigned character code that cannot be parsed as a `u32`
    #[error("{0} is not a valid character code")]
    InvalidCharacterCode(BString),
    /// A character code outside the ranges `0..=2147483647` and `-2147483648..-1`
    #[error("{0}")]
    CharacterCodeOutOfRange(u32),
    /// A FIGcharacter that has an empty row, without an *endmark*
    #[error("empty row in FIGcharacter {0}")]
    EmptyRow(u32),
}

/// A non-fatal issue with a FIGfont found while decoding
#[derive(Debug, Error, PartialEq, Eq)]
#[non_exhaustive]
pub enum FontWarning {
    /// The baseline parameter cannot be parsed as a `usize`.
    #[error(r#"could not parse "{0}" as the baseline parameter"#)]
    Baseline(BString),
    /// The baseline parameter is not between 1 and the height parameter (inclusive).
    #[error("baseline {baseline} not between 1 and {height} (height)")]
    BaselineOutOfRange {
        /// The baseline parameter
        baseline: usize,
        /// The height parameter
        height: NonZero<usize>,
    },
    /// A FIGcharacter contains a non-ASCII sub-character. This may cause issues with rendering and
    /// alignment, since the widths are measured in `char`s.
    #[error("the character with code {0} contains a non-ascii character on row {1}")]
    NonAscii(u32, usize),
    /// The font has fewer than the required 102 FIGcharacters
    #[error("Not enough required FIGcharacters, found {0}, expected 102")]
    MissingDefaultCharacters(usize),
    /// The font contains fewer tagged characters than specified in the header.
    #[error("found {found} tagged characters but expected {expected} from header")]
    TooFewCodeTags {
        /// The number of tagged characters in the font
        found: usize,
        /// The number of tagged characters specified in the header
        expected: usize,
    },
    /// A FIGcharacter has inconsistent width, which may cause unexpected rendering behavior.
    #[error("FIGcharacter with code {} has inconsistent width", Self::char_debug(*.0))]
    InconsistentWidth(u32),
    /// A FIGcharacter has a width greater than the maximum specified in the header.
    #[error("FIGcharacter with code {} has width {length} > {} (from header)", Self::char_debug(*.codepoint), .max_length)]
    ExcessLength {
        /// The character code
        codepoint: u32,
        /// The length of a row that is too wide
        length: usize,
        /// The maximum length of a row specified in the header
        max_length: usize,
    },
    /// The FIGfont contains data after the characters specified in the header.
    #[error("unexpected content after characters: {0}")]
    AfterCharacters(BString),
    /// A row in a FIGcharacter uses blank as an endmark. This likely indicates an extraneous
    /// trailing space, especially in combination with [`FontWarning::InconsistentWidth`].
    #[error("FIGcharacter with code {0} uses a blank as endmark")]
    BlankEndMark(u32),
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
    use super::{Font, FontFile};

    #[test]
    fn parse_standard() {
        use crate::render::test::{check_horizontal_standard, check_vertical_standard};

        let (font, warnings) = Font::new_with_warnings(Font::STANDARD).unwrap();
        assert!(warnings.is_empty());
        assert_eq!(font.header.hardblank, b'$');
        assert_eq!(font.header.height.get(), 6);
        assert_eq!(font.header.baseline, Some(5));
        assert_eq!(font.header.max_length, 16);

        check_horizontal_standard(font.header.horizontal_layout);
        check_vertical_standard(font.header.vertical_layout);
    }

    #[cfg(feature = "fonts")]
    #[test]
    fn parse_all() {
        for font in FontFile::ALL {
            let (_font, warnings) = Font::new_with_warnings(font.as_bytes())
                .unwrap_or_else(|e| panic!("failed to parse {font:?}: {e:?}"));
            assert_eq!(warnings, [], "warnings produced when parsing {font:?}",);
        }
    }
}
