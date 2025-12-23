//! A crate for parsing FIGfonts and rendering text using these fonts.
//!
//! # Features
//!
//! - Horizontal kerning/fitting and smushing
//!   ([`HorizontalSmushing`](crate::render::HorizontalSmushing))
//! - Vertical fitting and smushing ([`VerticalSmushing`](crate::render::VerticalSmushing))
//! - Alignment for multi-line output ([`Alignment`](crate::render::Alignment))
//! - Automatic line breaking depending on maximum width
//!   ([`Renderer::render`](crate::render::Renderer::render))
//!
//! # Example
//!
//! ```
//! # use letrs::font::Font;
//! # use letrs::render::{Renderer};
//! let rendered = Font::standard().render("Hello, world!");
//! let expected = concat!(
//! r" _   _      _ _                             _     _ _ ", "\n",
//! r"| | | | ___| | | ___    __      _____  _ __| | __| | |", "\n",
//! r"| |_| |/ _ \ | |/ _ \   \ \ /\ / / _ \| '__| |/ _` | |", "\n",
//! r"|  _  |  __/ | | (_) |   \ V  V / (_) | |  | | (_| |_|", "\n",
//! r"|_| |_|\___|_|_|\___( )   \_/\_/ \___/|_|  |_|\__,_(_)", "\n",
//! r"                    |/                                "
//! );
//! assert_eq!(rendered, expected);
//! ```
//!
//! ## Feature flags
//!
//! - `fonts` (default): adds the few dozen "standard" FIGfonts in the
//!   [`letrs-fonts`](https://crates.io/crates/letrs-fonts) package (via a dependency), which can be
//!   loaded using [`Font::built_in()`](crate::font::Font::built_in)

pub mod font;
pub mod render;
mod str_ext;
