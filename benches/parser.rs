use criterion::{criterion_group, criterion_main, Criterion};
use std::{fs, path::PathBuf, str::FromStr};

fn benchmark_quil_corpus(c: &mut Criterion) {
    from_corpus().iter().for_each(|cfg| {
        c.bench_function(&cfg.name, |b| {
            b.iter(|| {
                let _ = quil_rs::Program::from_str(&cfg.program);
            })
        });
    })
}

struct QuilBenchConfig {
    name: String,
    program: String,
}

fn from_corpus() -> Vec<QuilBenchConfig> {
    const PATH_SRC: &str = "benches/quilc/tests/good-test-files";

    // collect valid quil programs
    let mut programs = vec![];
    let mut corpus_dir = PathBuf::new();
    PATH_SRC.split('/').for_each(|p| corpus_dir.push(p));
    let dir = fs::read_dir(corpus_dir).expect("failed to locate quil corpus directory");

    dir.filter_map(Result::ok)
        .filter(|entry| {
            entry
                .metadata()
                .expect("failed to read file metadata")
                .is_file()
        })
        .for_each(|entry| {
            let program =
                fs::read_to_string(entry.path()).expect("failed to read quil program file");
            let name = entry
                .file_name()
                .to_str()
                .expect("bad filename")
                .to_string();

            // attempt to parse the quil once, ignoring unparsable input (only benchmark parsable code)
            if quil_rs::Program::from_str(&program).is_ok() {
                programs.push(QuilBenchConfig { name, program });
            }
        });

    programs
}

criterion_group!(benches, benchmark_quil_corpus);
criterion_main!(benches);
