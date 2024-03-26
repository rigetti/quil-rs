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
    Parse {
        #[arg(short = 't', long = "type")]
        input_type: Option<InputType>,
        input: String,
    },
}

#[derive(ValueEnum, Clone, Debug, Default)]
pub enum InputType {
    #[default]
    Program,
    Expression,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Parse { input_type, input } => {
            handle_parse(input_type.unwrap_or_default(), input)?
        }
    };

    Ok(())
}

fn handle_parse(input_type: InputType, input: String) -> anyhow::Result<()> {
    let parsed = match input_type {
        InputType::Program => {
            Program::from_str(&input).context("Failed to parse program from input string.")?.to_quil().expect("Parsed Program from valid Quil string, should be able to convert it back to valid Quil")
        }
        InputType::Expression => Expression::from_str(&input)
            .context("Failed to parse expression from input string.")?.to_quil().expect("Parsed Expression from valid Quil expression, should be able to convert it back to valid Expression"),
    };

    println!("{parsed}");

    Ok(())
}
