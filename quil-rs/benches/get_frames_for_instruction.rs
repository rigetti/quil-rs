mod corpus;

use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

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
                            let frames = prog.get_frames_for_instruction(instruction);
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
