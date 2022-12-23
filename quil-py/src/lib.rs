use pyo3::{create_exception, exceptions::PyRuntimeError, prelude::*, types::PyString};
use quil_rs::Program;
use rigetti_pyo3::{create_init_submodule, impl_repr, py_wrap_struct, PyWrapper, ToPython};

pub mod instruction;

create_exception!(quil, ParseError, PyRuntimeError);

// may need to define constructors "by hand", instead of imported macro
// gives full control
py_wrap_struct! {
    PyProgram(Program) as "Program" {
        py -> rs {
            string: PyString => Program {
                let native_program = string
                    .to_str()?
                    .parse::<quil_rs::Program>()
                    .map_err(|e| ParseError::new_err(e.to_string()))?;
                Ok::<_, PyErr>(native_program)
            }
        },
        rs -> py {
            program: Program => Py<PyString> { program.to_string(true).to_python(py) }
        }
    }
}
impl_repr!(PyProgram);

#[pymethods]
impl PyProgram {
    pub fn expand_calibrations(&self) -> PyResult<Self> {
        self.as_inner()
            .expand_calibrations()
            .map_err(|e| ParseError::new_err(e.to_string()))
            .map(PyProgram::from)
    }

    pub fn into_simplified(&self) -> PyResult<Self> {
        self.as_inner()
            .into_simplified()
            .map_err(|e| ParseError::new_err(e.to_string()))
            .map(PyProgram::from)
    }

    pub fn __str__(&self) -> PyResult<Py<PyString>> {
        self.clone().try_into()
    }
}

create_init_submodule! {
    classes: [ PyProgram ],
}

#[pymodule]
fn quil(py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule("quil", py, m)?;
    Ok(())
}

pub fn init_quil_submodule(name: &str, py: Python<'_>, m: &PyModule) -> PyResult<()> {
    init_submodule(name, py, m)?;
    Ok(())
}
