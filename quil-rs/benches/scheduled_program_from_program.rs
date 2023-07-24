use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use quil_rs::{instruction::InstructionHandler, program::graph::ScheduledProgram};
use std::str::FromStr;

mod corpus;

fn benchmark_quil_corpus(c: &mut Criterion) {
    let mut group = c.benchmark_group("ScheduleProgram::from_program");
    corpus::from_corpus()
        .iter()
        .filter(|cfg| {
            // Ignore any programs that would fail to schedule
            match quil_rs::Program::from_str(&cfg.program) {
                Err(_) => false,
                Ok(program) => {
                    ScheduledProgram::from_program(&program, &mut InstructionHandler::default())
                        .is_ok()
                }
            }
        })
        .for_each(|cfg| {
            group.bench_function(&cfg.name, |b| {
                b.iter_batched(
                    || quil_rs::Program::from_str(&cfg.program).unwrap(),
                    |program| {
                        let prog = ScheduledProgram::from_program(
                            &program,
                            &mut InstructionHandler::default(),
                        )
                        .expect("scheduling should not fail");
                        black_box(prog);
                    },
                    BatchSize::SmallInput,
                )
            });
        });
    group.finish();
}

criterion_group!(benches, benchmark_quil_corpus);
criterion_main!(benches);
