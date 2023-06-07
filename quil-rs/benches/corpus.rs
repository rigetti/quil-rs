use std::{fs, path::Path, process::Command, str::FromStr};

pub struct QuilBenchConfig {
    pub name: String,
    pub program: String,
}

pub fn from_corpus() -> Vec<QuilBenchConfig> {
    const PATH_SRC: &str = "benches/quilc/tests/good-test-files";

    // collect valid quil programs
    let mut programs = vec![];
    let corpus_dir = Path::new(PATH_SRC);
    if !corpus_dir.exists() {
        init_submodules()
    }

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

// in the event someone wants to run the benchmarks locally, this will download the corpus of quil used
fn init_submodules() {
    Command::new("git")
        .args(["submodule", "update", "--init", "--recursive"])
        .spawn()
        .expect("failed to spawn git process")
        .wait_with_output()
        .expect("failed to init submodules, verify `git` is installed");
}
