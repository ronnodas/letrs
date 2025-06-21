use enumset::{EnumSet, EnumSetType};
use thiserror::Error;

use crate::Hardblank;

pub type HorizontalLayout = Layout<HorizontalSmushing>;
pub type VerticalLayout = Layout<VerticalSmushing>;

#[derive(Clone, Copy, Debug)]
pub struct Layout<S: EnumSetType> {
    mode: LayoutMode,
    smushing: EnumSet<S>,
}

impl<S: EnumSetType> Layout<S> {
    pub(crate) const fn mode(&self) -> LayoutMode {
        self.mode
    }
}

impl HorizontalLayout {
    pub(crate) fn parse(
        old_layout: i8,
        full_layout: Option<u16>,
    ) -> Result<Self, LayoutParseError> {
        let layout = if let Some(full_layout) = full_layout {
            let [_, low] = full_layout.to_be_bytes();
            let default_mode = LayoutMode::parse(low >> 6).expect("u8 >> 6 is in 0..=3");
            match (old_layout, default_mode) {
                (0..=63, LayoutMode::Smushing)
                | (0, LayoutMode::Fitting)
                | (-1, LayoutMode::FullSize) => (),
                (-1..=63, _) => {
                    return Err(LayoutParseError::Inconsistent(old_layout, full_layout));
                }
                _ => return Err(LayoutParseError::InvalidOld(old_layout)),
            }
            let smushing =
                if old_layout >= 0 && u16::from(old_layout.cast_unsigned()) == full_layout & 63 {
                    HorizontalSmushing::parse(old_layout.cast_unsigned())
                } else {
                    return Err(LayoutParseError::Inconsistent(old_layout, full_layout));
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
                    let smushing = HorizontalSmushing::parse(old_layout.cast_unsigned());
                    Self {
                        mode: LayoutMode::Smushing,
                        smushing,
                    }
                }
                _ => return Err(LayoutParseError::InvalidOld(old_layout)),
            }
        };
        Ok(layout)
    }

    pub(crate) fn smush(
        self,
        end_char: char,
        start_char: char,
        hardblank: Hardblank,
    ) -> Option<char> {
        if let LayoutMode::FullSize | LayoutMode::Fitting = self.mode {
            return None;
        }
        if self.smushing.is_empty() {
            // universal smushing
            return if hardblank == start_char {
                Some(end_char)
            } else {
                Some(start_char)
            };
        }
        self.smushing
            .iter()
            .find_map(|smushing| smushing.smush(end_char, start_char, hardblank))
    }
}

impl VerticalLayout {
    pub(crate) fn parse(full_layout: Option<u16>) -> Result<Self, LayoutParseError> {
        let full_layout = full_layout.unwrap_or(0);
        let [high, _] = full_layout.to_be_bytes();
        let default_mode =
            LayoutMode::parse(high >> 5).ok_or(LayoutParseError::InvalidFull(full_layout))?;
        let smushing = VerticalSmushing::parse(high);
        Ok(Self {
            mode: default_mode,
            smushing,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LayoutMode {
    FullSize,
    Fitting,
    Smushing,
}

impl LayoutMode {
    const fn parse(two_bits: u8) -> Option<Self> {
        match two_bits {
            0 => Some(Self::FullSize),
            1 => Some(Self::Fitting),
            2 | 3 => Some(Self::Smushing),
            _ => None,
        }
    }
}

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u8")]
pub enum HorizontalSmushing {
    EqualCharacter = 0,
    Underscore = 1,
    Hierarchy = 2,
    OppositePair = 3,
    BigX = 4,
    Hardblank = 5,
}

impl HorizontalSmushing {
    fn parse(old_layout: u8) -> EnumSet<Self> {
        EnumSet::from_repr_truncated(old_layout)
    }

    fn smush(self, end: char, start: char, hardblank: Hardblank) -> Option<char> {
        match self {
            Self::EqualCharacter => (end == start && hardblank != start).then_some(start),
            Self::Underscore => {
                Self::underscore(start, end).or_else(|| Self::underscore(end, start))
            }
            Self::Hierarchy => Self::hierarchy(start, end).or_else(|| Self::hierarchy(end, start)),
            Self::OppositePair => matches!(
                (end, start),
                ('[', ']') | (']', '[') | ('{', '}') | ('}', '{') | ('(', ')') | (')', '(')
            )
            .then_some('|'),
            Self::BigX => match (end, start) {
                ('/', '\\') => Some('|'),
                ('\\', '/') => Some('Y'),
                ('>', '<') => Some('X'),
                _ => None,
            },
            Self::Hardblank => (hardblank == end && end == start).then_some(start),
        }
    }

    fn underscore(a: char, b: char) -> Option<char> {
        (matches!(
            b,
            '|' | '/' | '\\' | '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>'
        ) && a == '_')
            .then_some(b)
    }
    fn hierarchy(a: char, b: char) -> Option<char> {
        matches!(
            (a, b),
            (
                '|',
                '/' | '\\' | '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>'
            ) | ('/' | '\\', '[' | ']' | '{' | '}' | '(' | ')' | '<' | '>')
                | ('[' | ']', '{' | '}' | '(' | ')' | '<' | '>')
                | ('{' | '}', '(' | ')' | '<' | '>')
                | ('(' | ')', '<' | '>')
        )
        .then_some(b)
    }
}

#[derive(EnumSetType, Debug)]
#[enumset(repr = "u8")]
pub enum VerticalSmushing {
    EqualCharacter = 0,
    Underscore = 1,
    Hierarchy = 2,
    HorizontalLine = 3,
    VerticalLineSuper = 4,
}

impl VerticalSmushing {
    fn parse(high: u8) -> EnumSet<Self> {
        EnumSet::from_repr_truncated(high)
    }
}

#[derive(Error, Debug)]
pub enum LayoutParseError {
    #[error("inconsistent layout parameters: {0} and {1}")]
    Inconsistent(i8, u16),
    #[error("invalid `Old_Layout` {0}")]
    InvalidOld(i8),
    #[error("invalid `Full_Layout` {0}")]
    InvalidFull(u16),
}

#[cfg(test)]
pub(crate) mod test {
    use enumset::EnumSet;

    use crate::layout::HorizontalSmushing;

    use super::{HorizontalLayout, LayoutMode, VerticalLayout};

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
