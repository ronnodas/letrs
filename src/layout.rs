use enumset::{EnumSet, EnumSetType};
use thiserror::Error;

pub type HorizontalLayout = Layout<HorizontalSmushing>;
pub type VerticalLayout = Layout<VerticalSmushing>;

#[derive(Clone, Copy)]
pub struct Layout<S: EnumSetType> {
    default_mode: LayoutMode,
    smushing: EnumSet<S>,
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
                (-1, LayoutMode::FullWidth) => (),
                (0, LayoutMode::Fitting) => (),
                (0..=63, LayoutMode::Smushing) => (),
                (-1..=63, _) => {
                    return Err(LayoutParseError::Inconsistent(old_layout, full_layout));
                }
                _ => return Err(LayoutParseError::InvalidOld(old_layout)),
            };
            let smushing =
                if old_layout >= 0 && u16::from(old_layout.cast_unsigned()) == full_layout & 63 {
                    HorizontalSmushing::parse(old_layout.cast_unsigned())
                } else {
                    return Err(LayoutParseError::Inconsistent(old_layout, full_layout));
                };
            Self {
                default_mode,
                smushing,
            }
        } else {
            match old_layout {
                -1 => Self {
                    default_mode: LayoutMode::FullWidth,
                    smushing: EnumSet::empty(),
                },
                0 => Self {
                    default_mode: LayoutMode::Fitting,
                    smushing: EnumSet::empty(),
                },
                1..=63 => {
                    let smushing = HorizontalSmushing::parse(old_layout.cast_unsigned());
                    Self {
                        default_mode: LayoutMode::Smushing,
                        smushing,
                    }
                }
                _ => return Err(LayoutParseError::InvalidOld(old_layout)),
            }
        };
        Ok(layout)
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
            default_mode,
            smushing,
        })
    }
}

#[derive(Clone, Copy)]
pub enum LayoutMode {
    FullWidth,
    Fitting,
    Smushing,
}

impl LayoutMode {
    fn parse(two_bits: u8) -> Option<Self> {
        match two_bits {
            0 => Some(Self::FullWidth),
            1 => Some(Self::Fitting),
            2 | 3 => Some(Self::Smushing),
            _ => None,
        }
    }
}

#[derive(EnumSetType)]
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
    fn parse(old_layout: u8) -> EnumSet<HorizontalSmushing> {
        EnumSet::from_repr_truncated(old_layout)
    }
}

#[derive(EnumSetType)]
#[enumset(repr = "u8")]
pub enum VerticalSmushing {
    EqualCharacter = 0,
    Underscore = 1,
    Hierarchy = 2,
    HorizontalLine = 3,
    VerticalLineSuper = 4,
}

impl VerticalSmushing {
    fn parse(high: u8) -> EnumSet<VerticalSmushing> {
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
