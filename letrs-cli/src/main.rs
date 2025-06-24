//! TODO

use std::fmt::{self, Display};
use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, ValueEnum};
use letrs::font::Font;
use letrs::render::{PrintDirection, Renderer};

fn main() -> Result<()> {
    let cli = Cli::parse();
    let (font, warnings) = cli.font()?;
    let renderer = cli.renderer(&font);
    if let Some(output) = renderer.render(&cli.input, cli.width) {
        println!("{output}");
    } else {
        debug_assert!(
            warnings || cli.width + 2 < font.header().max_length,
            "rendering failed even though char width {} <= max width {}",
            font.header().max_length.saturating_sub(2),
            cli.width
        );
        println!(
            "Given width is too short, recommend at least {} for the given font",
            font.header().max_length.saturating_sub(2)
        );
    }
    Ok(())
}

#[derive(Parser)]
struct Cli {
    input: String,
    #[arg(short = 'f')]
    font: Option<PathBuf>,
    #[arg(short = 'w', default_value_t = 80)]
    width: usize,
    #[arg(short = 'd')]
    direction: Option<Direction>,
    #[arg(short = 'j', default_value_t)]
    alignment: Alignment,
}

impl Cli {
    fn font(&self) -> Result<(Font, bool)> {
        let result = if let Some(path) = &self.font {
            let (font, warnings) = Font::from_str_with_warnings(&fs::read_to_string(path)?)?;
            let any_warnings = !warnings.is_empty();
            for warning in warnings {
                println!("WARNING: {warning}");
            }
            (font, any_warnings)
        } else {
            (Font::standard(), false)
        };
        Ok(result)
    }

    fn renderer<'font>(&self, font: &'font Font) -> Renderer<'font> {
        let mut renderer = Renderer::new(font).alignment(self.alignment.into());
        if let Some(direction) = self.direction {
            renderer = renderer.print_direction(direction.into());
        }
        renderer
    }
}

#[derive(Clone, Copy, ValueEnum)]
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

#[derive(Clone, Copy, ValueEnum, Default)]
#[value(rename_all = "kebab-case")]
enum Alignment {
    #[default]
    Start,
    Center,
    End,
}

impl From<Alignment> for letrs::render::Alignment {
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
