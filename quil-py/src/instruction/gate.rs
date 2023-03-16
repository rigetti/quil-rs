use quil_rs::{
    expression::Expression,
    instruction::{Gate, GateDefinition, GateModifier, GateSpecification, Qubit},
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct, py_wrap_error, py_wrap_simple_enum,
    py_wrap_union_enum,
    pyo3::{
        exceptions::PyValueError,
        pyclass::CompareOp,
        pymethods,
        types::{PyInt, PyString},
        IntoPy, Py, PyObject, PyResult, Python,
    },
    wrap_error, PyTryFrom, PyWrapper, ToPython, ToPythonError,
};

use super::PyQubit;
use crate::expression::PyExpression;

wrap_error!(RustGateError(quil_rs::instruction::GateError));
py_wrap_error!(quil, RustGateError, GateError, PyValueError);

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyGate(Gate) as "Gate" {
        name: String => Py<PyString>,
        parameters: Vec<Expression> => Vec<PyExpression>,
        qubits: Vec<Qubit> => Vec<PyQubit>,
        modifiers: Vec<GateModifier> => Vec<PyGateModifier>
    }
}
impl_repr!(PyGate);
impl_str!(PyGate);

#[pymethods]
impl PyGate {
    #[new]
    fn new(
        py: Python<'_>,
        name: String,
        parameters: Vec<PyExpression>,
        qubits: Vec<PyQubit>,
        modifiers: Vec<PyGateModifier>,
    ) -> PyResult<Self> {
        Ok(Self(
            Gate::new(
                &name,
                Vec::<Expression>::py_try_from(py, &parameters)?,
                Vec::<Qubit>::py_try_from(py, &qubits)?,
                Vec::<GateModifier>::py_try_from(py, &modifiers)?,
            )
            .map_err(RustGateError::from)
            .map_err(RustGateError::to_py_err)?,
        ))
    }

    fn dagger(&self, py: Python<'_>) -> PyResult<Self> {
        self.as_inner().clone().dagger().to_python(py)
    }

    fn controlled(&self, py: Python<'_>, control_qubit: PyQubit) -> PyResult<Self> {
        self.as_inner()
            .clone()
            .controlled(Qubit::py_try_from(py, &control_qubit)?)
            .to_python(py)
    }

    fn forked(
        &self,
        py: Python<'_>,
        fork_qubit: PyQubit,
        params: Vec<PyExpression>,
    ) -> PyResult<Self> {
        self.as_inner()
            .clone()
            .forked(
                Qubit::py_try_from(py, &fork_qubit)?,
                Vec::<Expression>::py_try_from(py, &params)?,
            )
            .map_err(RustGateError::from)
            .map_err(RustGateError::to_py_err)?
            .to_python(py)
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyGateModifier(GateModifier) as "GateModifier" {
        Controlled,
        Dagger,
        Forked
    }
}
impl_repr!(PyGateModifier);
impl_str!(PyGateModifier);

#[pymethods]
impl PyGateModifier {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyGateSpecification(GateSpecification) as "GateSpecification" {
        matrix: Matrix => Vec<Vec<PyExpression>>,
        permutation: Permutation => Vec<Py<PyInt>>
    }
}
impl_repr!(PyGateSpecification);
impl_str!(PyGateSpecification);

#[pymethods]
impl PyGateSpecification {
    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

py_wrap_data_struct! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass)]
    PyGateDefinition(GateDefinition) as "GateDefinition" {
        name: String => Py<PyString>,
        parameters: Vec<String> => Vec<Py<PyString>>,
        specification: GateSpecification => PyGateSpecification
    }
}
impl_repr!(PyGateDefinition);
impl_str!(PyGateDefinition);

#[pymethods]
impl PyGateDefinition {
    #[new]
    pub fn new(
        py: Python<'_>,
        name: String,
        parameters: Vec<String>,
        specification: PyGateSpecification,
    ) -> PyResult<Self> {
        Ok(Self(GateDefinition::new(
            name,
            parameters,
            GateSpecification::py_try_from(py, &specification)?,
        )))
    }

    pub fn __richcmp__(&self, py: Python<'_>, other: &Self, op: CompareOp) -> PyObject {
        match op {
            CompareOp::Eq => (self.as_inner() == other.as_inner()).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}
