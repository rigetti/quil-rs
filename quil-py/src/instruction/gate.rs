use quil_rs::{
    expression::Expression,
    instruction::{Gate, GateDefinition, GateModifier, GateSpecification, Qubit},
};

use rigetti_pyo3::{
    impl_repr, impl_str, py_wrap_data_struct, py_wrap_error, py_wrap_union_enum,
    pyo3::{
        exceptions::PyValueError,
        pymethods,
        types::{PyInt, PyString},
        Py, PyResult, Python,
    },
    wrap_error, PyTryFrom, PyWrapper, ToPython, ToPythonError,
};

use super::{PyExpression, PyQubit};

wrap_error!(GateError(quil_rs::instruction::GateError));
py_wrap_error!(quil, GateError, PyGateError, PyValueError);

py_wrap_data_struct! {
    PyGate(Gate) as "Gate" with subclass {
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
            .map_err(GateError::from)
            .map_err(GateError::to_py_err)?,
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
            .map_err(GateError::from)
            .map_err(GateError::to_py_err)?
            .to_python(py)
    }
}

py_wrap_union_enum! {
    PyGateSpecification(GateSpecification) as "GateSpecification" {
        matrix: Matrix => Vec<Vec<PyExpression>>,
        permutation: Permutation => Vec<Py<PyInt>>
    }
}
impl_repr!(PyGateSpecification);

py_wrap_data_struct! {
    PyGateDefinition(GateDefinition) as "GateDefinition" {
        name: String => Py<PyString>,
        parameters: Vec<String> => Vec<Py<PyString>>,
        specification: GateSpecification => PyGateSpecification
    }
}
impl_repr!(PyGateDefinition);
impl_str!(PyGateDefinition);

py_wrap_union_enum! {
    PyGateModifier(GateModifier) as "GateModifier" {
        controlled: Controlled,
        dagger: Dagger,
        forked: Forked
    }
}
impl_repr!(PyGateModifier);
impl_str!(PyGateModifier);
