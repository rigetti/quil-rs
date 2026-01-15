use rigetti_pyo3::create_init_submodule;

mod validation {
    use rigetti_pyo3::create_init_submodule;

    create_init_submodule! {
        funcs: [ py_validate_program ],
    }

    #[cfg_attr(feature = "stubs", gen_stub_pyfunction(module = "quil.program.validation"))]
    #[pyfunction(name = "validate_program")]
    fn py_validate_program() -> PyResult<()> {
        Ok(())
    }
}

create_init_submodule! {
    classes: [ Program, CalibrationSet ],
    funcs: [ parse_program ],
    submodules: [
        "validation": validation::init_submodule,
    ],
}
