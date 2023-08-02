use criterion::{black_box, criterion_group, criterion_main, Criterion};
use once_cell::sync::Lazy;
use quil_rs::expression::Expression;
use std::str::FromStr;

static EXPRESSIONS: Lazy<Vec<(String, Expression)>> = Lazy::new(|| {
    include_str!("test_expressions.txt")
        .lines()
        .map(|line| {
            let (name, expr) = line
                .split_once('\t')
                .expect("these lines are designed this way");
            (
                name.to_string(),
                Expression::from_str(expr).expect("these are valid expressions"),
            )
        })
        .collect()
});

fn simplify(e: Expression) -> Expression {
    e.into_simplified()
}

fn benchmark_expression_simplification(c: &mut Criterion) {
    EXPRESSIONS.iter().for_each(|(n, e)| {
        c.bench_function(n, |b| b.iter(|| black_box(simplify(e.clone()))));
    })
}

criterion_group!(benches, benchmark_expression_simplification);
criterion_main!(benches);
