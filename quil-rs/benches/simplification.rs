use criterion::{black_box, criterion_group, criterion_main, Criterion};
use once_cell::sync::Lazy;
use quil_rs::expression::Expression;
use std::str::FromStr;

static EXPRESSIONS: Lazy<Vec<Expression>> = Lazy::new(|| {
    include_str!("expressions.txt")
        .lines()
        .map(|line| Expression::from_str(line).expect("these are valid expressions"))
        .collect()
});

fn simplify(e: Expression) -> Expression {
    e.into_simplified()
}

fn benchmark_expression_simplification(c: &mut Criterion) {
    EXPRESSIONS.iter().enumerate().for_each(|(i, e)| {
        c.bench_function(&format!("expression_{i}"), |b| {
            b.iter(|| black_box(simplify(e.clone())))
        });
    })
}

criterion_group!(benches, benchmark_expression_simplification);
criterion_main!(benches);
