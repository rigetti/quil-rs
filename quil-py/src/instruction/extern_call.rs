use quil_rs::instruction::{
    Call, ExternParameter, ExternParameterType, ExternSignature, ScalarType, UnresolvedCallArgument,
};

use rigetti_pyo3::{
    impl_hash, impl_repr, py_wrap_error, py_wrap_union_enum,
    pyo3::{pymethods, types::PyString, Py, PyResult, Python},
    wrap_error, PyTryFrom, ToPythonError,
};

use crate::{impl_copy_for_instruction, impl_eq, impl_pickle_for_instruction, impl_to_quil};

use super::{PyScalarType, PyVector};

wrap_error!(RustCallError(quil_rs::instruction::CallError));
py_wrap_error!(
    quil,
    RustCallError,
    CallError,
    rigetti_pyo3::pyo3::exceptions::PyValueError
);

wrap_error!(RustExternError(quil_rs::instruction::ExternError));
py_wrap_error!(
    quil,
    RustExternError,
    ExternError,
    rigetti_pyo3::pyo3::exceptions::PyValueError
);

rigetti_pyo3::py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyCall(Call) as "Call"
}

#[pymethods]
impl PyCall {
    #[new]
    fn new(name: String, arguments: Vec<PyCallArgument>) -> PyResult<Self> {
        Call::try_new(
            name,
            arguments.into_iter().map(PyCallArgument::into).collect(),
        )
        .map(Self)
        .map_err(RustCallError::from)
        .map_err(RustCallError::to_py_err)
    }

    #[getter]
    fn name(&self) -> &str {
        self.0.name()
    }

    #[getter]
    fn arguments(&self) -> Vec<PyCallArgument> {
        self.0
            .arguments()
            .iter()
            .map(PyCallArgument::from)
            .collect()
    }
}

rigetti_pyo3::impl_as_mut_for_wrapper!(PyCall);
impl_repr!(PyCall);
impl_to_quil!(PyCall);
impl_copy_for_instruction!(PyCall);
impl_hash!(PyCall);
impl_eq!(PyCall);
impl_pickle_for_instruction!(PyCall);

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyCallArgument(UnresolvedCallArgument) as "CallArgument" {
        identifier: Identifier => Py<PyString>,
        memory_reference: MemoryReference => super::PyMemoryReference,
        immediate: Immediate => Py<rigetti_pyo3::pyo3::types::PyComplex>
    }
}
impl_repr!(PyCallArgument);
impl_to_quil!(PyCallArgument);
impl_hash!(PyCallArgument);
impl_eq!(PyCallArgument);

py_wrap_union_enum! {
    #[derive(Debug, PartialEq, Eq)]
    PyExternParameterType(ExternParameterType) as "ExternParameterType" {
        scalar: Scalar => PyScalarType,
        fixed_length_vector: FixedLengthVector => PyVector,
        variable_length_vector: VariableLengthVector => PyScalarType
    }
}
impl_repr!(PyExternParameterType);
impl_to_quil!(PyExternParameterType);
impl_hash!(PyExternParameterType);
impl_eq!(PyExternParameterType);

rigetti_pyo3::py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyExternParameter(ExternParameter) as "ExternParameter"
}
rigetti_pyo3::impl_as_mut_for_wrapper!(PyExternParameter);
impl_repr!(PyExternParameter);
impl_to_quil!(PyExternParameter);
impl_copy_for_instruction!(PyExternParameter);
impl_hash!(PyExternParameter);
impl_eq!(PyExternParameter);
impl_pickle_for_instruction!(PyExternParameter);

#[pymethods]
impl PyExternParameter {
    #[new]
    fn new(
        py: Python<'_>,
        name: String,
        mutable: bool,
        data_type: PyExternParameterType,
    ) -> PyResult<Self> {
        ExternParameter::try_new(
            name,
            mutable,
            ExternParameterType::py_try_from(py, &data_type)?,
        )
        .map(Self)
        .map_err(RustExternError::from)
        .map_err(RustExternError::to_py_err)
    }

    #[getter]
    fn name(&self) -> &str {
        self.0.name()
    }

    #[getter]
    fn mutable(&self) -> bool {
        self.0.mutable()
    }

    #[getter]
    fn data_type(&self) -> PyExternParameterType {
        self.0.data_type().into()
    }
}

rigetti_pyo3::py_wrap_type! {
    #[derive(Debug, PartialEq, Eq)]
    #[pyo3(subclass, module = "quil.instructions")]
    PyExternSignature(ExternSignature) as "ExternSignature"
}
rigetti_pyo3::impl_as_mut_for_wrapper!(PyExternSignature);
impl_repr!(PyExternSignature);
impl_to_quil!(PyExternSignature);
impl_copy_for_instruction!(PyExternSignature);
impl_hash!(PyExternSignature);
impl_eq!(PyExternSignature);
impl_pickle_for_instruction!(PyExternSignature);

#[pymethods]
impl PyExternSignature {
    #[new]
    fn new(
        py: Python<'_>,
        parameters: Vec<PyExternParameter>,
        return_type: Option<PyScalarType>,
    ) -> PyResult<Self> {
        Ok(Self(ExternSignature::new(
            return_type
                .map(|scalar_type| ScalarType::py_try_from(py, &scalar_type))
                .transpose()?,
            Vec::<ExternParameter>::py_try_from(py, &parameters)?,
        )))
    }

    #[getter]
    fn parameters(&self) -> Vec<PyExternParameter> {
        self.0
            .parameters()
            .iter()
            .map(PyExternParameter::from)
            .collect()
    }

    #[getter]
    fn return_type(&self) -> Option<PyScalarType> {
        self.0.return_type().map(Into::into)
    }
}
