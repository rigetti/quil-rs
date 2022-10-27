use std::{fs, path::{Path, PathBuf}, io::Write};
use heck::ToSnekCase;

const TEST_FILE_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/benches/quilc/tests");

fn quil_files_in(folder: &Path) -> Vec<(String, String)> {
    println!("cargo:rerun-if-changed={}", folder.display());
    let mut list = Vec::new();
    for file in fs::read_dir(folder).unwrap_or_else(|err| panic!("failed to open {folder:?}: {err}")) {
        let file = file.expect("failed to access dir entry");
        let path = file.path();
        let meta = file.metadata().unwrap_or_else(|err| panic!("failed to read metadata for {path:?}: {err}"));
        let typ = meta.file_type();

        if typ.is_file() {
            let name = file.file_name().into_string().unwrap_or_else(|_| panic!("expected file name of {path:?} to be valid unicode"));
            let name = match name.strip_suffix(".quil") {
                Some(name) => name,
                None => continue,
            };
            let name = name.to_snek_case();
            let content = fs::read_to_string(&path).unwrap_or_else(|err| panic!("failed to read contents of {path:?}: {err}"));
            list.push((name.to_string(), content));
        } else if typ.is_dir() {
            list.append(&mut quil_files_in(&path));
        } else {
            panic!("symlinks are not supported!");
        }
    }
    list
}

fn test_file_path(dir: &str) -> PathBuf {
    PathBuf::from(TEST_FILE_ROOT).join(dir)
}

fn bad_files() -> Vec<(String, String)> {
    quil_files_in(&test_file_path("bad-test-files"))
}

fn good_files() -> Vec<(String, String)> {
    quil_files_in(&test_file_path("good-test-files"))
}

fn compiler_hook_files() -> Vec<(String, String)> {
    quil_files_in(&test_file_path("compiler-hook-test-files"))
}

const INDENT: &str = "    ";

fn write_tests_to_file(file: &mut fs::File, module: &str, should_succeed: bool, tests: &[(String, String)]) {
    let do_panic = |_| panic!("failed to write to generated tests file");
    writeln!(file, "mod {} {{", module).unwrap_or_else(do_panic);
    for (name, body) in tests {
        writeln!(file, "{}#[test]", INDENT).unwrap_or_else(do_panic);
        writeln!(file, "{}fn test_{}() {{", INDENT, name).unwrap_or_else(do_panic);
        writeln!(file, "{}{}const QUIL: &str = {:?};", INDENT, INDENT, body).unwrap_or_else(do_panic);
        if should_succeed {
            writeln!(file, "{}{}if let Err(error) = QUIL.parse::<quil_rs::Program>() {{", INDENT, INDENT).unwrap_or_else(do_panic);
            writeln!(file, "{}{}{}panic!(\"expected parsing to succeed, got: {{:#}}\", error);", INDENT, INDENT, INDENT).unwrap_or_else(do_panic);
            writeln!(file, "{}{}}}", INDENT, INDENT).unwrap_or_else(do_panic);
        } else {
            writeln!(file, "{}{}if let Ok(parsed) = QUIL.parse::<quil_rs::Program>() {{", INDENT, INDENT).unwrap_or_else(do_panic);
            writeln!(file, "{}{}{}panic!(\"expected parsing to fail, got: {{:#?}}\", parsed);", INDENT, INDENT, INDENT).unwrap_or_else(do_panic);
            writeln!(file, "{}{}}}", INDENT, INDENT).unwrap_or_else(do_panic);
        }
        writeln!(file, "{}}}", INDENT).unwrap_or_else(do_panic);
    }
    writeln!(file, "}}").unwrap_or_else(do_panic);
}

fn main() {
    let path = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("quilc_corpus_tests.rs");
    let mut file = fs::File::create(&path).unwrap_or_else(|err| panic!("failed to create {}: {}", path.display(), err));

    write_tests_to_file(&mut file, "good", true, &good_files());
    write_tests_to_file(&mut file, "bad", true, &bad_files());
    write_tests_to_file(&mut file, "compiler_hook", true, &compiler_hook_files());

    file.sync_all().unwrap_or_else(|err| panic!("failed to sync {path:?} to disk: {err}"));
}
