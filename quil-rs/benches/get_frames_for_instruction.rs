mod corpus;

use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quil_rs::instruction::{DefaultHandler, InstructionHandler as _};

fn benchmark_quil_corpus(c: &mut Criterion) {
    corpus::from_corpus().iter().for_each(|cfg| {
        c.bench_function(&cfg.name, |b| {
            b.iter_batched(
                || {
                    quil_rs::Program::from_str(&cfg.program)
                        .expect("program should parse successfully")
                },
                |prog| {
                    for instruction in prog.body_instructions() {
                        for _ in 0..50 {
                            let frames = DefaultHandler.matching_frames(&prog, instruction);
                            black_box(frames);
                        }
                    }
                },
                criterion::BatchSize::SmallInput,
            )
        });
    })
}

criterion_group!(benches, benchmark_quil_corpus);
criterion_main!(benches);
