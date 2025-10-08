use crate::{ some_import, another_import };

#[pymodule]
#[pyo3(name = "instructions", module = "quil", submodule)]
pub(crate) fn init_submodule(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();

    m.add("InstructionError", py.get_type::<InstructionError>())?;
    m.add("SomeKindOfError", py.get_type::<SomeKindOfError>())?;
    m.add_function(wrap_pyfunction!(a_different_name, m)?)?;
    
    m.add_class::<EnumA>()?;
    m.add_class::<EnumB>()?;
    m.add_class::<SimpleEnum>()?;

    fix_complex_enums!(
        py,
        EnumA,
        EnumB,
    );

    Ok(())
}

#[cfg_attr(feature = "stubs", gen_stub_pyfunction(module = "quil.instructions"))]
#[pyfunction(name = "some_function")]
fn a_different_name() -> PyResult<()> {
    Ok(())
}

create_exception!(
    quil.instructions,
    InstructionError,
    QuilError,
    "Base error type for errors related to ``Instruction`` processing."
);

exception!(
    crate::somecode::RustError,
    quil.instructions,
    SomeKindOfError,
    InstructionError,
    "Errors of some sort."
);

impl_instruction!([
    InstA,
    InstB,
    InstC,
]);

instruction_getnewargs!(
    InstA,
    InstB,
);

#[cfg_attr(not(feature = "stubs"), optipy::strip_pyo3(only_stubs))]
#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstC {
    #[gen_stub(override_return_type(type_repr = "tuple[int | float]"))]
    fn __getnewargs__<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        match self {
            Self::LiteralInteger(value) => (value,).into_pyobject(py),
            Self::LiteralReal(value) => (value,).into_pyobject(py),
        }
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstA {
    #[getter]
    fn name(&self) -> &str {
        &self.identifier.name
    }
}

pickleable_new! {
    impl InstB {
        fn __new__(name: String) -> Result<InstB, SubmodError>;
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstC {
    fn arguments(&self) -> Vec<Args> {
        self.arguments.clone()
    }
}

#[cfg_attr(feature = "stubs", gen_stub_pymethods)]
#[pymethods]
impl InstA {
    #[staticmethod]
    fn parse(input: &str) -> Result<Self, ParseInstructionError> {
        <Self as std::str::FromStr>::from_str(input)
            .map_err(|err| ParseInstructionError::Parse(err.to_string()))
    }
}

