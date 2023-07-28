use std::fs::read_to_string;
use std::str::FromStr;

use rstest::rstest;

use quil_rs::program::Program;

#[rstest]
#[case("cz")]
#[case("cz_phase")]
#[case("measure")]
#[case("rx")]
#[case("xy")]
fn test_duplicate_calibrations(#[case] program_name: &str) {
    // TODO: Create a common module that loads test programs
    let program_path = format!(
        "{}/tests/programs/calibration_{program_name}.quil",
        env!("CARGO_MANIFEST_DIR")
    );
    let program_text = read_to_string(&program_path).unwrap_or_else(|_| {
        panic!(
            "Should be able to locate test file: {}",
            program_path.clone()
        )
    });

    let program = Program::from_str(&program_text).expect("Should be able to parse test program.");

    // TODO: Snapshot test
    assert_eq!(program_text, program.to_string());
}
