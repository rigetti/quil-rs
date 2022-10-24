use criterion::{criterion_group, criterion_main, Criterion};
use std::{fs, str::FromStr};

fn benchmark_sample_calibration(c: &mut Criterion) {
    let input = fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/benches/sample-calibrations.quil"))
        .expect("benches/sample-calibrations.quil should exist");

    let mut group = c.benchmark_group("calibration file");
    group.sample_size(100);
    group.bench_function("Sample calibration file", |b| {
        b.iter(|| {
            let _ = quil_rs::Program::from_str(&input);
        })
    });
    group.finish();
}

criterion_group!(benches, benchmark_sample_calibration);
criterion_main!(benches);

