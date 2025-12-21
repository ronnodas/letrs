//! FIGfonts
//!
//! Font types and the logic for parsing `.flf` files.

use std::collections::HashMap;
use std::num::NonZero;
use std::str::{self, FromStr};

use bstr::{BString, ByteSlice as _};
use itertools::Itertools as _;
#[cfg(feature = "fonts")]
pub use letrs_fonts::FontFile;
use thiserror::Error;

use crate::render::{HorizontalLayout, LayoutDecodeError, Renderer, VerticalLayout};

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
    /// [`Font::from_str_with_warnings`]; this method is (currently) a convenience wrapper around
    /// that, ignoring the warnings. Notably, if the font has FIGcharacters that do not use the same
    /// number of sub-characters (bytes) per row, the rendering algorithm may behave unexpectedly,
    /// but this is only emitted as a warning (when using [`Font::from_str_with_warnings`]) and not
    /// a fatal error.
    ///
    /// # Errors
    /// Returns `Err` on a fatal decoding error; see [`FontError`] for details.
    fn from_bytes(font: impl AsRef<[u8]>) -> Result<Self, FontError> {
        Self::from_bytes_with_warnings(font).map(|(font, _)| font)
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
    pub fn from_bytes_with_warnings(
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
        let header = Header::decode(header_line, &mut warnings)?;
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
        Self::from_bytes(Self::STANDARD).expect("Should be tested")
    }

    /// Decodes a FIGfont from the `letrs-fonts` crate.
    ///
    /// Only available with the `fonts` feature.
    #[expect(clippy::missing_panics_doc, reason = "should be caught in tests")]
    #[cfg(feature = "fonts")]
    #[must_use]
    pub fn built_in(font: FontFile) -> Self {
        Self::from_bytes(font.as_bytes()).expect("Should be tested")
    }

    /// Renders a string with default settings provided by the font and no `max_width`.
    #[must_use]
    pub fn render(&self, string: &str) -> String {
        Renderer::new(self).render_unbounded(string)
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

/// A FIGfont header.
#[derive(Clone, Copy, Debug)]
pub struct Header {
    /// The *hardblank* character; see [`Hardblank`] and
    /// [`HorizontalSmushing`](crate::render::HorizontalSmushing) for its significance in rendering.
    pub hardblank: Hardblank,
    /// Number of rows of sub-characters in each FIGcharacter. Note that *every* FIGcharacter in a
    /// given FIGfont has the same height, and this includes any empty space above or below the
    /// glyph.
    pub height: NonZero<usize>,
    /// The number of lines of sub-characters from the baseline of a FIGcharacter to the top of the
    /// tallest FIGcharacter. The baseline of a FIGfont is an imaginary line on top of which capital
    /// letters would rest, while the *descenders* of characters such as lowercase g, j, p, q, and y
    /// may hang below. In other words, Baseline is the height of a FIGcharacter, ignoring any
    /// descenders.
    ///
    /// This parameter does not affect the rendered output.
    ///
    /// Should be between 1 and [`height`](Header::height) inclusive; see
    /// [`FontWarning::BaselineOutOfRange`].
    pub baseline: usize,
    /// An upper bound for the length of each row of each FIGcharacter in the font. This is expected
    /// to be the width of the widest FIGcharacter, plus 2 (to accommodate *endmarks*); see
    /// [`FontWarning::ExcessLength`]. Use [`Font::max_width`] if you need the actual maximum width.
    pub max_length: usize,
    /// Number of lines of comments between the header and the FIGcharacters. See also
    /// [`Font::comments`].
    pub comment_lines: usize,
    /// The default horizontal layout and smushing modes
    pub horizontal_layout: HorizontalLayout,
    /// The default vertical layout and smushing modes
    pub vertical_layout: VerticalLayout,
    /// The default print direction (left-to-right or right-to-left).
    pub print_direction: PrintDirection,
    /// Number of characters provided by the font outside of the standard 102 (see
    /// [`DEFAULT_CODEPOINTS`]), plus any tags with *negative* codes that do not correspond to
    /// actual FIGcharacters but contain human-readable information, usually translation tables. See
    /// [`Font::ignored_characters`].
    pub code_tag_count: usize,
}

impl Header {
    pub(crate) fn decode(
        header_line: &[u8],
        warnings: &mut Vec<FontWarning>,
    ) -> Result<Self, HeaderError> {
        let mut parameters = header_line
            .split(|&c| c == b' ')
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
            return Err(HeaderError::NotEnoughParameters(BString::new(
                header_line.to_owned(),
            )));
        };
        let print_direction = parameters.next();
        let full_layout = parameters.next();
        let code_tag_count = parameters.next();
        let Some(hardblank) = signature_and_hardblank.strip_prefix(b"flf2a") else {
            return Err(HeaderError::UnknownSignature(
                signature_and_hardblank.into(),
            ));
        };
        let Ok(hardblank) = hardblank.bytes().exactly_one() else {
            return Err(HeaderError::HardblankLength(hardblank.into()));
        };
        let hardblank = hardblank
            .try_into()
            .map_err(HeaderError::InvalidHardblankChar)?;
        let Some(height) = NonZero::new(IntParameter::Height.parse(height)?) else {
            return Err(HeaderError::ZeroHeight);
        };
        let baseline = IntParameter::Baseline.parse(baseline).unwrap_or_else(|_| {
            warnings.push(FontWarning::Baseline(baseline.into()));
            1
        });
        if !(0 < baseline && baseline <= height.get()) {
            warnings.push(FontWarning::BaselineOutOfRange { baseline, height });
        }
        let max_length = IntParameter::MaxLength.parse(max_length)?;
        let comment_lines = IntParameter::CommentLines.parse(comment_lines)?;
        let old_layout = IntParameter::OldLayout.parse(old_layout)?;
        let full_layout = full_layout
            .map(|full_layout| IntParameter::FullLayout.parse(full_layout))
            .transpose()?;
        let horizontal_layout = HorizontalLayout::decode(old_layout, full_layout)?;
        let vertical_layout = VerticalLayout::decode(full_layout)?;
        let print_direction = PrintDirection::decode(print_direction)?;
        let code_tag_count = code_tag_count
            .map(|count| IntParameter::CodeTagCount.parse(count))
            .transpose()?
            .unwrap_or(0);
        let header = Self {
            hardblank,
            height,
            baseline,
            max_length,
            comment_lines,
            horizontal_layout,
            vertical_layout,
            print_direction,
            code_tag_count,
        };
        Ok(header)
    }
}

/// Printing direction, left-to-right or right-to-left
///
/// Each font specifies a default, found in `font.header().print_direction`. This also affects the
/// meaning of [`Alignment`](crate::render::Alignment).
///
/// The rendered string should always be interpreted left-to-right.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PrintDirection {
    /// Left-to-right
    LeftToRight,
    /// Right-to-left
    RightToLeft,
}

impl PrintDirection {
    pub(crate) fn decode(print_direction: Option<&[u8]>) -> Result<Self, HeaderError> {
        Ok(match print_direction {
            None | Some(b"0") => Self::LeftToRight,
            Some(b"1") => Self::RightToLeft,
            Some(other) => return Err(HeaderError::PrintDirection(other.into())),
        })
    }
}

#[derive(Debug, Clone, Copy)]
enum IntParameter {
    Height,
    Baseline,
    MaxLength,
    CommentLines,
    OldLayout,
    FullLayout,
    CodeTagCount,
}

impl IntParameter {
    fn parse<T: FromStr>(self, bytes: &[u8]) -> Result<T, HeaderError> {
        str::from_utf8(bytes)
            .ok()
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| HeaderError::Parse(self.name(), bytes.into()))
    }

    const fn name(self) -> &'static str {
        match self {
            Self::Height => "Height",
            Self::Baseline => "Baseline",
            Self::MaxLength => "Max_Length",
            Self::CommentLines => "Comment_Lines",
            Self::OldLayout => "Old_Layout",
            Self::FullLayout => "Full_Layout",
            Self::CodeTagCount => "Codetag_Count",
        }
    }
}

/// A hardblank character
///
/// A hardblank is a special sub-character which is displayed as a blank (`' '`) once rendered, but
/// is treated more like a visible sub-character when fitting or smushing *horizontally*. Therefore,
/// hardblanks keep adjacent FIGcharacters a certain distance apart.
///
/// The usual hardblank is a `$`, but it can be any character except a blank (`' '`), a
/// carriage-return, a newline or a null character.
///
/// See [`HorizontalSmushing`](crate::render::HorizontalSmushing) and [The FIGfont
/// standard](http://www.jave.de/figlet/figfont.html#hardblanks) for more details.
#[derive(Clone, Copy, Debug)]
pub struct Hardblank(u8);

impl PartialEq<u8> for Hardblank {
    fn eq(&self, other: &u8) -> bool {
        self.0 == *other
    }
}

impl TryFrom<u8> for Hardblank {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if matches!(value, b' ' | b'\r' | b'\n' | 0) {
            Err(value)
        } else {
            Ok(Self(value))
        }
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

/// An error in decoding a FIGfont header
#[derive(Debug, Error)]
pub enum HeaderError {
    #[error("missing header")]
    /// There is no header, ie the contents are empty.
    Missing,
    /// The header has fewer than the five required parameters (after the signature and hardblank).
    #[error(r#""{0}" does not include enough parameters"#)]
    NotEnoughParameters(BString),
    /// The header does not begin with `"flf2a"`.
    #[error(r#"{0} does not begin with "flf2a""#)]
    UnknownSignature(BString),
    /// The hardblank is either missing or contains more than one character.
    #[error(r#"hardblank "{0}" is not exactly one character"#)]
    HardblankLength(BString),
    /// The specified hardblank is not a blank (space), a carriage-return, a newline (linefeed)
    /// or a null byte.
    #[error("'{0}' must not be the hardblank")]
    InvalidHardblankChar(u8),
    /// One of the integer parameters cannot be parsed.
    #[error("{1} cannot be parsed as the parameter `{0}`")]
    Parse(&'static str, BString),
    /// The print direction parameter is not 0 or 1.
    #[error(r#""{0}" is an invalid print direction, expecting 0 or 1"#)]
    PrintDirection(BString),
    /// An error decoding the layout parameters
    #[error("{0}")]
    Layout(#[from] LayoutDecodeError),
    /// The height parameter is 0
    #[error("height parameter is 0")]
    ZeroHeight,
}

/// A non-fatal issue with a FIGfont found while decoding
#[derive(Debug, Error, PartialEq, Eq)]
#[non_exhaustive]
pub enum FontWarning {
    /// A FIGcharacter contains a non-ASCII sub-character. This may cause issues with rendering and
    /// alignment, since the widths are measured in `char`s.
    #[error("the character with code {0} contains a non-ascii character on row {1}")]
    NonAscii(u32, usize),
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

        let (font, warnings) = Font::from_bytes_with_warnings(Font::STANDARD).unwrap();
        assert!(warnings.is_empty());
        assert_eq!(font.header.hardblank, b'$');
        assert_eq!(font.header.height.get(), 6);
        assert_eq!(font.header.baseline, 5);
        assert_eq!(font.header.max_length, 16);

        check_horizontal_standard(font.header.horizontal_layout);
        check_vertical_standard(font.header.vertical_layout);
    }

    #[cfg(feature = "fonts")]
    #[test]
    fn parse_all() {
        for font in FontFile::ALL {
            let (_font, warnings) = Font::from_bytes_with_warnings(font.as_bytes())
                .unwrap_or_else(|e| panic!("failed to parse {font:?}: {e:?}"));
            assert_eq!(warnings, [], "warnings produced when parsing {font:?}",);
        }
    }
}
