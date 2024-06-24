use std::{
    fs,
    path::{Path, PathBuf},
    process::Command,
    str::FromStr,
};

pub struct QuilBenchConfig {
    pub name: String,
    pub program: String,
}

fn bench_config_from_file(path: &Path) -> Option<QuilBenchConfig> {
    if !path.is_file() {
        return None;
    }

    let program = fs::read_to_string(path).expect("failed to read quil program file");
    let name = path
        .file_name()
        .expect("path should have file name component")
        .to_str()
        .expect("filename should be valid")
        .to_string();

    if quil_rs::Program::from_str(&program).is_ok() {
        Some(QuilBenchConfig { name, program })
    } else {
        None
    }
}

pub fn from_corpus() -> Vec<QuilBenchConfig> {
    const PATH_SRC: &str = "benches/quilc/tests/good-test-files";
    const SAMPLE_CALIBRATIONS: &str = "benches/sample-calibrations.quil";

    // collect valid quil programs
    let corpus_dir = Path::new(PATH_SRC);
    if !corpus_dir.exists() {
        init_submodules()
    }

    let dir = fs::read_dir(corpus_dir).expect("failed to locate quil corpus directory");

    dir.filter_map(Result::ok)
        .filter_map(|entry| bench_config_from_file(&entry.path()))
        .chain(bench_config_from_file(&PathBuf::from(SAMPLE_CALIBRATIONS)))
        .collect()
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
