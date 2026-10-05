//! Rust-side cost of `Program`'s body-instruction storage.
//!
//! Compare `cargo bench --bench body_instructions` (no Python cache field)
//! with `cargo bench --bench body_instructions --features stubs` (with it).

use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quil_rs::{instruction::Instruction, Program};

const N: usize = 10_000;

fn source() -> String {
    (0..N)
        .map(|i| format!("RX(pi/2) {}", i % 8))
        .collect::<Vec<_>>()
        .join("\n")
}

fn benchmark_body_instructions(c: &mut Criterion) {
    let source = source();
    let program = Program::from_str(&source).expect("valid program");
    let instructions: Vec<Instruction> = program.body_instructions().cloned().collect();

    let mut group = c.benchmark_group("body instructions (n = 10k gates)");
    group.bench_function("parse", |b| {
        b.iter(|| Program::from_str(black_box(&source)).unwrap())
    });
    group.bench_function("clone", |b| b.iter(|| black_box(&program).clone()));
    group.bench_function("add_instruction x 10k", |b| {
        b.iter_batched(
            || instructions.clone(),
            |instructions| {
                let mut p = Program::new();
                for instruction in instructions {
                    p.add_instruction(instruction);
                }
                p
            },
            criterion::BatchSize::LargeInput,
        )
    });
    group.bench_function("iterate body", |b| {
        b.iter(|| black_box(&program).body_instructions().count())
    });
    group.bench_function("resolve_placeholders", |b| {
        b.iter_batched(
            || program.clone(),
            |mut p| {
                p.resolve_placeholders();
                p
            },
            criterion::BatchSize::LargeInput,
        )
    });
    group.finish();
}

criterion_group!(benches, benchmark_body_instructions);
criterion_main!(benches);
