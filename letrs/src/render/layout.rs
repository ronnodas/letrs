use enumset::{EnumSet, EnumSetType};
use thiserror::Error;

use crate::font::Hardblank;

/// Combination of a layout mode and a set of horizontal smushing modes
pub type HorizontalLayout = Layout<HorizontalSmushing>;
/// Combination of a layout mode and a set of vertical smushing modes
///
/// Vertical [fitting](LayoutMode::Fitting) or [smushing](LayoutMode::Smushing) moves entire lines
/// of FIGcharacters at once.
pub type VerticalLayout = Layout<VerticalSmushing>;

/// Combination of a layout mode and a set of smushing modes
///
/// The smushing modes are only relevant if the layout mode is [`LayoutMode::Smushing`] (possibly by
/// overriding the font default using methods on [`Renderer`](crate::render::Renderer)).
#[derive(Clone, Copy, Debug)]
pub struct Layout<S: EnumSetType> {
    mode: LayoutMode,
    smushing: EnumSet<S>,
}

impl<S: EnumSetType> Layout<S> {
    /// Returns the layout mode.
    pub const fn mode(&self) -> LayoutMode {
        self.mode
    }

    /// Sets the layout mode.
    pub const fn set_mode(&mut self, mode: LayoutMode) {
        self.mode = mode;
    }

    /// Returns true if the given smushing mode is active.
    pub fn smushing_mode_active(&self, mode: S) -> bool {
        self.smushing.contains(mode)
    }

    /// Returns true if *universal smushing* is active.
    ///
    /// When universal smushing, sub-characters from an earlier FIGcharacter are overridden by
    /// sub-characters from a later FIGcharacter (except if the later subcharacter is a
    /// [*hardblank*](Hardblank)). This produces an "overlapping" effect with some FIGfonts, whereby
    /// later FIGcharacter may appear to be in front.
    ///
    /// Returns `true` if and only if `smushing_mode_active(mode)` returns `false` for every mode.
    pub fn universal_smushing(&self) -> bool {
        self.smushing.is_empty()
    }
}

impl HorizontalLayout {
    /// Decode the layout parameters and return a [`HorizontalLayout`].
    ///
    /// This only uses the *low* byte of `full_layout` (if available). In more detail, `old_layout`
    /// should be either `-1`, signifying [`LayoutMode::FullSize`], or its bits correspond to:
    /// * bit 0: [`HorizontalSmushing::EqualCharacter`]
    /// * bit 1: [`HorizontalSmushing::Underscore`]
    /// * bit 2: [`HorizontalSmushing::Hierarchy`]
    /// * bit 3: [`HorizontalSmushing::OppositePair`]
    /// * bit 4: [`HorizontalSmushing::BigX`]
    /// * bit 5: [`HorizontalSmushing::Hardblank`]
    /// * bits 6 and 7: must not be set
    ///
    /// If none of the bits 0--5 are set then [`LayoutMode::Fitting`] is used, unless overridden by
    /// `full_layout`. If any of them are set then [`LayoutMode::Smushing`] is implied.
    ///
    /// Similarly, the low 8 bits of `full_layout` correspond to:
    /// * bit 0: [`HorizontalSmushing::EqualCharacter`]
    /// * bit 1: [`HorizontalSmushing::Underscore`]
    /// * bit 2: [`HorizontalSmushing::Hierarchy`]    
    /// * bit 3: [`HorizontalSmushing::OppositePair`]
    /// * bit 4: [`HorizontalSmushing::BigX`]
    /// * bit 5: [`HorizontalSmushing::Hardblank`]  
    /// * bit 6: [`LayoutMode::Fitting`]
    /// * bit 7: [`LayoutMode::Smushing`], overriding bit 6 if both are set
    ///
    /// If none of the bits 0--5 are set but bit 7 is set, then *universal smushing* is active. If
    /// neither of bits 6 and 7 are set, [`LayoutMode::FullSize`] is implied.
    ///
    /// ```
    /// # use letrs::render::{HorizontalLayout, HorizontalSmushing, LayoutMode};
    /// let layout = HorizontalLayout::decode(0b001111, Some(0b0101_1111_1000_1111)).unwrap();
    /// assert_eq!(layout.mode(), LayoutMode::Smushing);
    ///
    /// assert!(layout.smushing_mode_active(HorizontalSmushing::EqualCharacter)); // active
    /// assert!(layout.smushing_mode_active(HorizontalSmushing::Underscore));     // active
    /// assert!(layout.smushing_mode_active(HorizontalSmushing::Hierarchy));      // active
    /// assert!(layout.smushing_mode_active(HorizontalSmushing::OppositePair));   // active
    /// assert!(!layout.smushing_mode_active(HorizontalSmushing::BigX));          // not active
    /// assert!(!layout.smushing_mode_active(HorizontalSmushing::Hardblank));     // not active
    /// ```
    ///
    /// # Errors
    /// See [`LayoutDecodeError`] for possible errors.
    ///
    /// This function does not check that `full_layout <= 32767`.
    #[expect(clippy::missing_panics_doc, reason = "cannot actually panic")]
    pub fn decode(old_layout: i8, full_layout: Option<u16>) -> Result<Self, LayoutDecodeError> {
        let layout = if let Some(full_layout) = full_layout {
            let [_, low] = full_layout.to_be_bytes();
            let default_mode = LayoutMode::decode(low >> 6).expect("u8 >> 6 is in 0..=3");
            match (old_layout, default_mode) {
                (0..=63, LayoutMode::Smushing)
                | (0, LayoutMode::Fitting)
                | (-1, LayoutMode::FullSize) => (),
                (-1..=63, _) => {
                    return Err(LayoutDecodeError::Inconsistent(old_layout, full_layout));
                }
                _ => return Err(LayoutDecodeError::InvalidOld(old_layout)),
            }
            let smushing = match u8::try_from(old_layout) {
                Ok(smushing) if smushing == low & 63 => HorizontalSmushing::decode(smushing),
                Err(_) => HorizontalSmushing::decode(low & 63),
                _ => return Err(LayoutDecodeError::Inconsistent(old_layout, full_layout)),
            };
            Self {
                mode: default_mode,
                smushing,
            }
        } else {
            match old_layout {
                -1 => Self {
                    mode: LayoutMode::FullSize,
                    smushing: EnumSet::empty(),
                },
                0 => Self {
                    mode: LayoutMode::Fitting,
                    smushing: EnumSet::empty(),
                },
                1..=63 => {
                    let smushing = HorizontalSmushing::decode(old_layout.unsigned_abs());
                    Self {
                        mode: LayoutMode::Smushing,
                        smushing,
                    }
                }
                _ => return Err(LayoutDecodeError::InvalidOld(old_layout)),
            }
        };
        Ok(layout)
    }

    pub(crate) fn smush(self, end: u8, start: u8, hardblank: Hardblank) -> Option<u8> {
        if let LayoutMode::FullSize | LayoutMode::Fitting = self.mode {
            return None;
        }
        if self.smushing.is_empty() {
            // universal smushing
            return if hardblank == start {
                Some(end)
            } else {
                Some(start)
            };
        }
        self.smushing
            .iter()
            .find_map(|smushing| smushing.smush(end, start, hardblank))
    }
}

impl VerticalLayout {
    /// Decode the `full_layout` parameter and return a [`VerticalLayout`].
    ///
    /// This only uses the *high* byte of `full_layout` (if `Some`). In more detail, the high 8 bits
    /// of `full_layout` correspond to:
    /// * bit 0: [`VerticalSmushing::EqualCharacter`]
    /// * bit 1: [`VerticalSmushing::Underscore`]
    /// * bit 2: [`VerticalSmushing::Hierarchy`]    
    /// * bit 3: [`VerticalSmushing::HorizontalLine`]
    /// * bit 4: [`VerticalSmushing::VerticalLineSuper`]
    /// * bit 5: [`LayoutMode::Fitting`]
    /// * bit 6: [`LayoutMode::Smushing`], overriding bit 5 if both are set
    /// * bit 7: must not be set
    ///
    /// If none of the bits 0--4 are set but bit 6 is set, then *universal smushing* is active. If
    /// neither of bits 6 and 7 are set, [`LayoutMode::FullSize`] is implied.
    ///
    /// ```
    /// # use letrs::render::{VerticalLayout, VerticalSmushing, LayoutMode};
    /// let layout = VerticalLayout::decode(Some(0b0101_1111_1000_1111)).unwrap();
    /// assert_eq!(layout.mode(), LayoutMode::Smushing);
    ///
    /// assert!(layout.smushing_mode_active(VerticalSmushing::EqualCharacter));    // active
    /// assert!(layout.smushing_mode_active(VerticalSmushing::Underscore));        // active
    /// assert!(layout.smushing_mode_active(VerticalSmushing::Hierarchy));         // active
    /// assert!(layout.smushing_mode_active(VerticalSmushing::HorizontalLine));    // active
    /// assert!(layout.smushing_mode_active(VerticalSmushing::VerticalLineSuper)); // active
    /// ```
    ///
    /// If `full_layout` is `None`, the default is [`LayoutMode::FullSize`] (and *universal
    /// smushing* in case the layout mode is overridden via
    /// [`Renderer::vertical_layout`](crate::render::Renderer::vertical_layout)).
    /// ```
    /// # use letrs::render::{VerticalLayout, VerticalSmushing, LayoutMode};
    /// let layout = VerticalLayout::decode(None).unwrap();
    /// assert_eq!(layout.mode(), LayoutMode::FullSize);
    /// ```
    ///
    /// # Errors
    /// Can only emit [`LayoutDecodeError::InvalidFull`], precisely when `full_layout > 32767`.
    pub fn decode(full_layout: Option<u16>) -> Result<Self, LayoutDecodeError> {
        let full_layout = full_layout.unwrap_or(0);
        let [high, _] = full_layout.to_be_bytes();
        let default_mode =
            LayoutMode::decode(high >> 5).ok_or(LayoutDecodeError::InvalidFull(full_layout))?;
        let smushing = VerticalSmushing::decode(high);
        Ok(Self {
            mode: default_mode,
            smushing,
        })
    }

    pub(crate) fn smush(self, end: u8, start: u8) -> Option<u8> {
        if let LayoutMode::FullSize | LayoutMode::Fitting = self.mode {
            return None;
        }
        if self.smushing.is_empty() {
            // universal smushing
            return Some(end);
        }
        self.smushing
            .iter()
            .find_map(|smushing| smushing.smush(end, start))
    }

    pub(crate) fn super_smushing(self) -> bool {
        self.mode == LayoutMode::Smushing
            && self.smushing.contains(VerticalSmushing::VerticalLineSuper)
    }
}

/// Layout modes, used for both horizontal and vertical layout
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LayoutMode {
    /// No kerning, the FIGcharacters are laid out as rectangles.
    FullSize,
    /// FIGcharacters are moved closer until they touch, which means that two non-blank
    /// sub-characters are adjacent.
    ///
    /// Note that hardblanks count as blanks for *vertical* layout but *not* for horizontal layout.
    /// See also [`Hardblank`].
    Fitting,
    /// FIGcharacters are moved one step[^super] closer after they touch, if possible. Which
    /// sub-characters are allowed to collide is controlled by the *smushing modes*, specified by
    /// the font. In case of *universal smushing*, later FIGcharacters are given priority.
    /// Otherwise, it depends on the set of *controlled smushing* modes that are active (see
    /// [`HorizontalSmushing`] and [`VerticalSmushing`]). If some pair of overlapping sub-characters
    /// cannot be smushed, [fitting](LayoutMode::Fitting) occurs instead.
    ///
    /// [^super]: Except for vertical layout with [`VerticalSmushing::VerticalLineSuper`] active.
    Smushing,
}

impl LayoutMode {
    const fn decode(two_bits: u8) -> Option<Self> {
        match two_bits {
            0 => Some(Self::FullSize),
            1 => Some(Self::Fitting),
            2 | 3 => Some(Self::Smushing),
            _ => None,
        }
    }
}

/// Controlled smushing modes for horizontal layout
#[derive(EnumSetType, Debug)]
#[enumset(repr = "u8")]
pub enum HorizontalSmushing {
    /// Smush two sub-characters if they are the same, but not a [*hardblank*](Hardblank).
    ///
    /// Same as [`VerticalSmushing::EqualCharacter`], except for hardblanks.
    EqualCharacter = 0,
    /// An underscore (`_`) will be overridden by any of: `|`, `/`, `\`, `[`, `]`, `{`, `}`, `(`,
    /// `)`, `<` or `>`.
    ///
    /// Same as [`VerticalSmushing::Underscore`].
    Underscore = 1,
    /// A hierarchy of six classes is used: `|`, `/\`, `[]`, `{}`, `()`, and `<>`. When two smushing
    /// sub-characters are from different classes, the one from the latter class will be used.
    ///
    /// Same as [`VerticalSmushing::Hierarchy`].
    Hierarchy = 2,
    /// Smushes opposing brackets (`[]`), braces (`{}`) and parentheses (`()`) together, replacing
    /// any such pair with a vertical bar (`|`).
    ///
    /// Note that the pairs are smushed in either order, but they must be of the same kind.
    OppositePair = 3,
    /// Smushes `/\` into `|`, `\/` into `Y`, and `><` into `X`.
    ///
    /// Note that here the pairs *are* ordered, in particular `<>` is not affected by this rule.
    BigX = 4,
    /// Two [hardblanks](Hardblank) are smushed to a single one.
    Hardblank = 5,
}

impl HorizontalSmushing {
    fn decode(bits: u8) -> EnumSet<Self> {
        EnumSet::from_repr_truncated(bits)
    }

    fn smush(self, end: u8, start: u8, hardblank: Hardblank) -> Option<u8> {
        match self {
            Self::EqualCharacter => (end == start && hardblank != start).then_some(start),
            Self::Underscore => underscore(start, end).or_else(|| underscore(end, start)),
            Self::Hierarchy => hierarchy(start, end).or_else(|| hierarchy(end, start)),
            Self::OppositePair => matches!(
                (end, start),
                (b'[', b']')
                    | (b']', b'[')
                    | (b'{', b'}')
                    | (b'}', b'{')
                    | (b'(', b')')
                    | (b')', b'(')
            )
            .then_some(b'|'),
            Self::BigX => match (end, start) {
                (b'/', b'\\') => Some(b'|'),
                (b'\\', b'/') => Some(b'Y'),
                (b'>', b'<') => Some(b'X'),
                _ => None,
            },
            Self::Hardblank => (hardblank == end && end == start).then_some(start),
        }
    }
}

/// Controlled smushing modes for vertical layout
///
/// Vertical smushing moves entire lines of FIGcharacters at once.
#[derive(EnumSetType, Debug)]
#[enumset(repr = "u8")]
pub enum VerticalSmushing {
    /// Smush two sub-characters if they are the same.
    ///
    /// Same as [`HorizontalSmushing::EqualCharacter`], except treats hardblanks as blanks.
    EqualCharacter = 0,
    /// An underscore (`_`) will be overridden by any of: `|`, `/`, `\`, `[`, `]`, `{`, `}`, `(`,
    /// `)`, `<` or `>`.
    ///
    /// Same as [`HorizontalSmushing::Underscore`].
    Underscore = 1,
    /// A hierarchy of six classes is used: `|`, `/\`, `[]`, `{}`, `()`, and `<>`.
    ///
    /// When two smushing sub-characters are from different classes, the one from the latter class
    /// will be used.
    ///
    /// Same as [`HorizontalSmushing::Hierarchy`].
    Hierarchy = 2,
    /// Smushes stacked pairs of `-` and `_`, replacing them with a single `=`.
    ///
    /// It does not matter which is found above the other, but they have to be *different*, so this
    /// is independent from [`VerticalSmushing::EqualCharacter`].
    HorizontalLine = 3,
    /// This one rule is different from all others, in that it *supersmushes* vertical lines
    /// consisting of several vertical bars (`|`). This creates the illusion that FIGcharacters have
    /// slid vertically against each other. Supersmushing continues until any sub-characters other
    /// than `|` would have to be smushed. Supersmushing can produce impressive results, but it is
    /// seldom possible, since other sub-characters would usually have to be considered for smushing
    /// as soon as any such stacked vertical lines are encountered.
    VerticalLineSuper = 4,
}

impl VerticalSmushing {
    fn decode(bits: u8) -> EnumSet<Self> {
        EnumSet::from_repr_truncated(bits)
    }

    fn smush(self, end: u8, start: u8) -> Option<u8> {
        match self {
            Self::EqualCharacter => (end == start).then_some(end),
            Self::Underscore => underscore(start, end).or_else(|| underscore(end, start)),
            Self::Hierarchy => hierarchy(start, end).or_else(|| hierarchy(end, start)),
            Self::HorizontalLine => {
                matches!((start, end), (b'_', b'-') | (b'-', b'_')).then_some(b'=')
            }
            Self::VerticalLineSuper => None,
        }
    }
}

fn underscore(a: u8, b: u8) -> Option<u8> {
    (matches!(
        b,
        b'|' | b'/' | b'\\' | b'[' | b']' | b'{' | b'}' | b'(' | b')' | b'<' | b'>'
    ) && a == b'_')
        .then_some(b)
}

fn hierarchy(a: u8, b: u8) -> Option<u8> {
    matches!(
        (a, b),
        (
            b'|',
            b'/' | b'\\' | b'[' | b']' | b'{' | b'}' | b'(' | b')' | b'<' | b'>'
        ) | (
            b'/' | b'\\',
            b'[' | b']' | b'{' | b'}' | b'(' | b')' | b'<' | b'>'
        ) | (b'[' | b']', b'{' | b'}' | b'(' | b')' | b'<' | b'>')
            | (b'{' | b'}', b'(' | b')' | b'<' | b'>')
            | (b'(' | b')', b'<' | b'>')
    )
    .then_some(b)
}

/// Errors that can occur when decoding layout parameters
///
/// Depending on the font encoding version, two parameters `Old_Layout` and `Full_Layout` are
/// considered, each encoding a set of flags as bits (except when `Old_Layout = -1`).
///
/// See [`HorizontalLayout::decode`] and [`VerticalLayout::decode`] for more details on the
/// encoding.
#[derive(Error, Debug)]
pub enum LayoutDecodeError {
    /// The two parameters describe inconsistent layout modes.
    ///
    /// This is only relevant for decoding a [`HorizontalLayout`], since `Old_Layout` does not
    /// contain information about [`VerticalLayout`].
    #[error("inconsistent layout parameters: {0} and {1}")]
    Inconsistent(i8, u16),
    /// The `Old_Layout` parameter is outside the range `-1..=63`.
    #[error("invalid `Old_Layout` {0}")]
    InvalidOld(i8),
    /// The `Full_Layout` parameter is outside the range `0..=32767`.
    #[error("invalid `Full_Layout` {0}")]
    InvalidFull(u16),
}

#[cfg(test)]
pub(crate) mod test {
    use enumset::EnumSet;

    use super::{HorizontalLayout, HorizontalSmushing, LayoutMode, VerticalLayout};

    pub(crate) fn check_horizontal_standard(layout: HorizontalLayout) {
        assert_eq!(layout.mode, LayoutMode::Smushing);
        let expected_smushing = HorizontalSmushing::EqualCharacter
            | HorizontalSmushing::Underscore
            | HorizontalSmushing::Hierarchy
            | HorizontalSmushing::OppositePair;
        assert_eq!(layout.smushing, expected_smushing);
    }

    pub(crate) fn check_vertical_standard(layout: VerticalLayout) {
        assert_eq!(layout.mode, LayoutMode::Smushing);
        assert_eq!(layout.smushing, EnumSet::all());
    }
}
