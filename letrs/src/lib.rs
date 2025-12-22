//! A crate for parsing FIGfonts and rendering text using these fonts.
//!
//! # Features
//! - Horizontal kerning/fitting and smushing
//! - Vertical fitting and smushing
//! - Alignment for multi-line output
//! - Automatic line breaking depending on maximum width
//!
//! # Possible future features
//! - Control files
//! - Performance, both speed and memory usage
//! - TOIlet fonts
//!
//! # Example
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

pub mod font;
pub mod render;
mod str_ext;
