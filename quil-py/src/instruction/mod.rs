use std::str::FromStr;

use pyo3::{pyfunction, types::PyList, PyResult, Python};
use quil_rs::instruction::{Instruction, Instructions};
use rigetti_pyo3::{impl_repr, py_wrap_type, py_wrap_union_enum, ToPython};

use self::{arithmetic::PyArithmetic, declaration::PyDeclaration};

pub mod arithmetic;
pub mod binary_logic;
pub mod calibration;
pub mod declaration;
pub mod expression;
pub mod frame;
pub mod gate;
pub mod memory_reference;
pub mod memory_region;
pub mod qubit;
pub mod waveform;

py_wrap_type! {
    #[derive(Debug)]
    PyInstructions(Vec<Instruction>) as "Instructions";
}

// TODO: Error handling
#[pyfunction]
pub fn parse_instructions<'a>(py: Python<'a>, input: &str) -> PyResult<&'a PyList> {
    Ok(PyList::new(
        py,
        Instructions::from_str(input)
            .unwrap()
            .0
            .iter()
            .map(|i| i.to_python(py))
            .collect::<PyResult<Vec<PyInstruction>>>()?,
    ))
}

// TODO: This _may_ work after every associated type has python bindings
py_wrap_union_enum! {
    PyInstruction(Instruction) as "Instruction" {
        halt: Halt,
        nop: Nop,
        arithmetic: Arithmetic => PyArithmetic,
        // gate: Gate => PyGate
        declaration: Declaration => PyDeclaration
    }
}

impl_repr!(PyInstruction);
