mod corpus;

use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_quil_corpus(c: &mut Criterion) {
    corpus::from_corpus().iter().for_each(|cfg| {
        for include_blocked in [true, false] {
            c.bench_function(
                &format!(
                    "{} ({}include blocked)",
                    cfg.name,
                    if include_blocked { "" } else { "no " }
                ),
                |b| {
                    b.iter_batched(
                        || {
                            quil_rs::Program::from_str(&cfg.program)
                                .expect("program should parse successfully")
                        },
                        |prog| {
                            for instruction in prog.body_instructions() {
                                for _ in 0..50 {
                                    let frames = prog
                                        .get_frames_for_instruction(instruction, include_blocked);
                                    black_box(frames);
                                }
                            }
                        },
                        criterion::BatchSize::SmallInput,
                    )
                },
            );
        }
    })
}

criterion_group!(benches, benchmark_quil_corpus);
criterion_main!(benches);
