#![allow(clippy::print_stdout, reason = "cli output")]

//! CLI for the [`letrs`](https://crates.io/crates/letrs) library
//!
//! Run as `letrs input` for default settings or use `letrs --help` to see options.

use std::fmt::{self, Display};
use std::fs;

use anyhow::Result;
use clap::{Parser, ValueEnum};
use letrs::font::{Font, FontFile, PrintDirection};
use letrs::render::{self, Bounded, Renderer};
use print_bytes::println_lossy;

fn main() -> Result<()> {
    let cli = Cli::parse();
    let (font, warnings) = cli.font()?;
    let success = if cli.binary_output {
        cli.renderer(&font)
            .render::<Vec<u8>>(&cli.input)
            .is_some_and(|output| {
                println_lossy(&output);
                true
            })
    } else {
        cli.renderer(&font)
            .render::<String>(&cli.input)
            .is_some_and(|output| {
                println!("{output}");
                true
            })
    };
    if !success {
        debug_assert!(
            warnings || cli.width < font.max_width(),
            "rendering failed even though char width {} <= max width {}",
            font.max_width(),
            cli.width
        );
        eprintln!(
            "Given width is too short, recommend at least {} for the given font",
            font.max_width()
        );
    }
    Ok(())
}

#[derive(Debug, Parser)]
#[command(version, about)]
struct Cli {
    input: String,
    #[arg(
        short = 'f',
        help = "Name of a built-in font, or the path to a font file [default: standard]"
    )]
    font: Option<String>,
    #[arg(short = 'w', default_value_t = 80)]
    width: usize,
    #[arg(short = 'd')]
    direction: Option<Direction>,
    #[arg(short = 'j', default_value_t)]
    alignment: Alignment,
    #[arg(short = 'l')]
    horizontal_mode: Option<LayoutMode>,
    #[arg(short = 'v')]
    vertical_mode: Option<LayoutMode>,
    #[arg(
        short = 'b',
        help = "Print binary output directly, without replacing non-UTF-8 characters"
    )]
    binary_output: bool,
}

impl Cli {
    fn font(&self) -> Result<(Font, bool)> {
        let result = if let Some(font) = &self.font {
            if let Some(font) = FontFile::from_name(font) {
                (Font::built_in(font), false)
            } else {
                let (font, warnings) = Font::new_with_warnings(&fs::read(font)?)?;
                let any_warnings = !warnings.is_empty();
                for warning in warnings {
                    println!("WARNING: {warning}");
                }
                (font, any_warnings)
            }
        } else {
            (Font::standard(), false)
        };
        Ok(result)
    }

    fn renderer<'font>(&self, font: &'font Font) -> Renderer<'font, Bounded> {
        let mut renderer = Renderer::new(font)
            .alignment(self.alignment.into())
            .max_width(self.width);
        if let Some(direction) = self.direction {
            renderer = renderer.print_direction(direction.into());
        }
        if let Some(layout_mode) = self.horizontal_mode {
            renderer = renderer.horizontal_layout(layout_mode.into());
        }
        if let Some(layout_mode) = self.vertical_mode {
            renderer = renderer.vertical_layout(layout_mode.into());
        }
        renderer
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[value(rename_all = "kebab-case")]
enum Direction {
    LeftToRight,
    RightToLeft,
}

impl From<Direction> for PrintDirection {
    fn from(value: Direction) -> Self {
        match value {
            Direction::LeftToRight => Self::LeftToRight,
            Direction::RightToLeft => Self::RightToLeft,
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum, Default)]
#[value(rename_all = "kebab-case")]
enum Alignment {
    #[default]
    Start,
    Center,
    End,
}

impl From<Alignment> for render::Alignment {
    fn from(value: Alignment) -> Self {
        match value {
            Alignment::Start => Self::Start,
            Alignment::Center => Self::Center,
            Alignment::End => Self::End,
        }
    }
}

impl Display for Alignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Start => write!(f, "start"),
            Self::Center => write!(f, "center"),
            Self::End => write!(f, "end"),
        }
    }
}

#[derive(Debug, Clone, Copy, ValueEnum)]
#[value(rename_all = "kebab-case")]
enum LayoutMode {
    Full,
    Fit,
    Smush,
}

impl From<LayoutMode> for render::LayoutMode {
    fn from(value: LayoutMode) -> Self {
        match value {
            LayoutMode::Full => Self::FullSize,
            LayoutMode::Fit => Self::Fitting,
            LayoutMode::Smush => Self::Smushing,
        }
    }
}
