use std::num::NonZero;
use std::str::{self, FromStr};

use bstr::{BString, ByteSlice as _};
use itertools::Itertools as _;
use thiserror::Error;

use crate::render::{HorizontalLayout, LayoutDecodeError, VerticalLayout};

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
    /// If `None` then this parameter could not be parsed as a non-negative integer; see
    /// [`FontWarning::Baseline`](crate::font::FontWarning::Baseline). If `Some` then the value
    /// Should be between 1 and [`height`](Header::height) inclusive; see
    /// [`FontWarning::BaselineOutOfRange`](`crate::font::FontWarning::BaselineOutOfRange`).
    pub baseline: Option<usize>,
    /// An upper bound for the length of each row of each FIGcharacter in the font. This is expected
    /// to be the width of the widest FIGcharacter, plus 2 (to accommodate *endmarks*); see
    /// [`FontWarning::ExcessLength`](crate::font::FontWarning::ExcessLength). Use
    /// [`Font::max_width`](crate::font::Font::max_width) if you need the actual maximum width.
    pub max_length: usize,
    /// Number of lines of comments between the header and the FIGcharacters. See also
    /// [`Font::comments`](crate::font::Font::comments).
    pub comment_lines: usize,
    /// The default horizontal layout and smushing modes
    pub horizontal_layout: HorizontalLayout,
    /// The default vertical layout and smushing modes
    pub vertical_layout: VerticalLayout,
    /// The default print direction (left-to-right or right-to-left).
    pub print_direction: PrintDirection,
    /// Number of characters provided by the font outside of the standard 102 (see
    /// [`DEFAULT_CODEPOINTS`](crate::font::DEFAULT_CODEPOINTS)), plus any tags with *negative*
    /// codes that do not correspond to actual FIGcharacters but contain human-readable information,
    /// usually translation tables. See
    /// [`Font::ignored_characters`](crate::font::Font::ignored_characters).
    pub code_tag_count: usize,
}

impl Header {
    /// Decode a header from its binary encoding.
    ///
    /// # Errors
    /// Any fatal decoding errors, see [`HeaderError`] for details.
    pub fn decode(header_line: impl AsRef<[u8]>) -> Result<Self, HeaderError> {
        Self::decode_inner(header_line.as_ref()).map(|(header, _)| header)
    }

    pub(crate) fn decode_inner(header_line: &[u8]) -> Result<(Self, Option<BString>), HeaderError> {
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
        let (baseline, bad_baseline) = IntParameter::Baseline.parse(baseline).map_or_else(
            |_| (None, Some(baseline.into())),
            |baseline| (Some(baseline), None),
        );

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
        Ok((header, bad_baseline))
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
