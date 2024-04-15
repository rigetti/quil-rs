use anyhow::Context;
use clap::{Parser, Subcommand, ValueEnum};
use quil_rs::{expression::Expression, quil::Quil, Program};
use std::str::FromStr;

#[derive(Parser, Debug)]
struct Cli {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand, Clone, Debug)]
pub enum Command {
    /// Parse a Quil program or expression
    Parse {
        #[arg(short = 't', long = "type", value_enum, default_value_t)]
        input_type: InputType,
        input: String,
    },
}

#[derive(ValueEnum, Clone, Debug, Default)]
pub enum InputType {
    /// Parse a Quil program (default)
    #[default]
    Program,
    /// Parse a Quil expression
    Expression,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Parse { input_type, input } => handle_parse(input_type, input)?,
    };

    Ok(())
}

fn handle_parse(input_type: InputType, input: String) -> anyhow::Result<()> {
    let parsed = match input_type {
        InputType::Program => {
            Program::from_str(&input)
                .context("Failed to parse program from input string.")?
                .to_quil()
                .context("Parsed Program from valid Quil string, but was unable to convert it back to valid Quil. This is probably a bug in the quil-rs parser.")?
        }
        InputType::Expression => {
             Expression::from_str(&input)
                .context("Failed to parse expression from input string.")?
                .to_quil()
                .context("Parsed Expression from valid Quil expression, but was unable to convert it back to valid Expression. This is probably a bug in the quil-rs parser.")?
        }
    };

    println!("{parsed}");

    Ok(())
}
