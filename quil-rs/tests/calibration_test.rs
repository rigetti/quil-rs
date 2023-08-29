use std::fs::read_to_string;
use std::path::PathBuf;
use std::str::FromStr;

use insta::assert_snapshot;
use rstest::rstest;

use quil_rs::program::Program;
use quil_rs::quil::Quil;

/// A test to ensure that a Program doesn't incorrectly filter out calibrations
/// when it is parsed.
#[rstest]
fn test_calibration_filtering(#[files("tests/programs/calibration_*.quil")] path: PathBuf) {
    let program_text =
        read_to_string(&path).unwrap_or_else(|_| panic!("Should be able to load file: {:?}", path));
    let program = Program::from_str(&program_text).expect("Should be able to parse test program.");

    assert_snapshot!(
        path.file_name().unwrap().to_str(),
        program.to_quil_or_debug()
    );
}
