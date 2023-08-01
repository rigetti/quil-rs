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

fn benchmark_expression_simplification(c: &mut Criterion) {
    EXPRESSIONS.iter().enumerate().for_each(|(i, e)| {
        let mut e2 = e.clone();
        c.bench_function(&format!("expression_{i}"), |b| {
            b.iter(|| black_box(e2.simplify()))
        });
    })
}

criterion_group!(benches, benchmark_expression_simplification);
criterion_main!(benches);
