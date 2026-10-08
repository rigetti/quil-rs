//! Generate a file full of expressions for benchmarking simplification.
//!
//! Expressions will be written to a specified file as "<HASH>\t<EXPR>", where <HASH> is the hex
//! value corresponding to the hashed version of the expression, and <EXPR> is an overly
//! parenthesized string version of the expression to ensure consistent parsing.

use clap::Parser;
use quil_rs::{
    expression::{
        Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
        PrefixExpression, PrefixOperator,
    },
    instruction::MemoryReference,
    quil::Quil,
    reserved::ReservedToken,
};
use rand::{distributions::Alphanumeric, rngs::StdRng, Rng, SeedableRng};
use std::{
    collections::hash_map::DefaultHasher,
    fs::File,
    hash::{Hash, Hasher},
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

/// Default output path for writing the test expressions.
fn get_default_output_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("benches")
        .join("test_expressions.txt")
}

/// Generate a file full of expressions for benchmarking simplification.
#[derive(Parser)]
struct Args {
    /// Output path
    #[arg(short, long, default_value=get_default_output_path().into_os_string())]
    output_path: PathBuf,
    /// Number of Expressions to generate
    #[arg(short, long, default_value_t = 25)]
    number_of_expressions: u64,
    /// Maximum depth of expressions
    #[arg(short, long, default_value_t = 10)]
    maximum_depth: u64,
    /// Seed for PRNG (default from random.org)
    #[arg(short, long, default_value_t = 27586845)]
    seed: u64,
}

/// Parenthesized version of [`Expression::to_string()`] to ensure consistent re-parsing.
fn parenthesized(expression: &Expression) -> String {
    use Expression::*;
    match expression {
        Address(memory_reference) => format!("({memory_reference})"),
        FunctionCall(FunctionCallExpression {
            function,
            expression,
        }) => format!("({function}({}))", parenthesized(expression)),
        Infix(InfixExpression {
            left,
            operator,
            right,
        }) => format!(
            "({}{}{})",
            parenthesized(left),
            operator,
            parenthesized(right)
        ),
        Number(_) => format!("({})", expression.to_quil_or_debug()),
        PiConstant => "pi".to_string(),
        Prefix(PrefixExpression {
            operator,
            expression,
        }) => format!("({}{})", operator, parenthesized(expression)),
        Variable(identifier) => format!("(%{identifier})"),
    }
}

/// Build a random [`Expression`]
fn build(rng: &mut impl Rng, depth: u64) -> Expression {
    if depth == 0 {
        match rng.gen_range(0..4) {
            0 => addr(rng),
            1 => number(rng),
            2 => Expression::PiConstant,
            3 => var(rng),
            _ => unreachable!(),
        }
    } else {
        let d = depth - 1;
        match rng.gen_range(0..3) {
            0 => func(rng, d),
            1 => infix(rng, d),
            2 => prefix(rng, d),
            _ => unreachable!(),
        }
    }
}

/// Random usable name.
fn name(rng: &mut impl Rng) -> String {
    let len = rng.gen_range(1..=10);
    let mut name = rng
        .sample_iter(&Alphanumeric)
        .take(len)
        .map(char::from)
        .collect::<String>();
    while ReservedToken::from_str(&name).is_ok()
        || name.to_lowercase() == "nan"
        || name
            .chars()
            .next()
            .map(|c| c.is_ascii_digit())
            .unwrap_or(false)
    {
        name = rng
            .sample_iter(&Alphanumeric)
            .take(len)
            .map(char::from)
            .collect();
    }
    name
}

/// Random address.
fn addr(rng: &mut impl Rng) -> Expression {
    Expression::Address(MemoryReference {
        name: name(rng),
        index: rng.gen(),
    })
}

/// Random complex number.
fn number(rng: &mut impl Rng) -> Expression {
    Expression::Number(num_complex::Complex::<f64> {
        re: rng.gen(),
        im: rng.gen(),
    })
}

/// Random variable.
fn var(rng: &mut impl Rng) -> Expression {
    Expression::Variable(name(rng))
}

/// Random function.
fn func(rng: &mut impl Rng, depth: u64) -> Expression {
    let function = match rng.gen_range(0..5) {
        0 => ExpressionFunction::Cis,
        1 => ExpressionFunction::Cosine,
        2 => ExpressionFunction::Exponent,
        3 => ExpressionFunction::Sine,
        4 => ExpressionFunction::SquareRoot,
        _ => unreachable!(),
    };
    Expression::FunctionCall(FunctionCallExpression {
        function,
        expression: build(rng, depth).into(),
    })
}

/// Random infix expression.
fn infix(rng: &mut impl Rng, depth: u64) -> Expression {
    let operator = match rng.gen_range(0..5) {
        0 => InfixOperator::Caret,
        1 => InfixOperator::Plus,
        2 => InfixOperator::Minus,
        3 => InfixOperator::Slash,
        4 => InfixOperator::Star,
        _ => unreachable!(),
    };
    Expression::Infix(InfixExpression {
        left: build(rng, depth).into(),
        operator,
        right: build(rng, depth).into(),
    })
}

/// Random prefix expression.
fn prefix(rng: &mut impl Rng, depth: u64) -> Expression {
    let operator = if rng.gen() {
        PrefixOperator::Plus
    } else {
        PrefixOperator::Minus
    };
    Expression::Prefix(PrefixExpression {
        operator,
        expression: build(rng, depth).into(),
    })
}

/// Hash a hashable thing to a number.
fn hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

/// Generate a file full of expressions for benchmarking simplification.
fn main() -> Result<(), String> {
    let args = Args::parse();
    let mut rng = StdRng::seed_from_u64(args.seed);
    let file = File::create(args.output_path)
        .map_err(|e| format!("Error in creating output file: {e:?}"))?;
    let mut buf = BufWriter::new(file);
    for _ in 0..args.number_of_expressions {
        let e = build(&mut rng, args.maximum_depth);
        let h = hash(&e);
        writeln!(buf, "{:x}\t{}", h, parenthesized(&e),)
            .map_err(|e| format!("Error in writing to output file: {e:?}"))?;
    }
    Ok(())
}
