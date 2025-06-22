use std::fmt::{self, Display};
use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::{Parser, ValueEnum};
use letrs::render::{self, Renderer};
use letrs::{Font, PrintDirection};

fn main() -> Result<()> {
    let cli = Cli::parse();
    let font = cli.font()?;
    let output = cli.renderer(&font).render(&cli.input);
    println!("{output}");
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
    fn font(&self) -> Result<Font> {
        let font = match &self.font {
            Some(path) => {
                let (font, warnings) = Font::parse_strict(&fs::read_to_string(path)?)?;
                for warning in warnings {
                    println!("WARNING: {warning}");
                }
                font
            }

            None => Font::standard(),
        };
        Ok(font)
    }

    fn renderer<'font>(&self, font: &'font Font) -> Renderer<'font> {
        let mut renderer = Renderer::new(font)
            .max_width(self.width)
            .alignment(self.alignment.into());
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
