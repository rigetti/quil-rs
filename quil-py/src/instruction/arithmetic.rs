use quil_rs::instruction::{
    Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand, BinaryOperands,
    BinaryOperator, MemoryReference,
};

use rigetti_pyo3::{
    impl_as_mut_for_wrapper, impl_repr, impl_str, py_wrap_data_struct, py_wrap_simple_enum,
    py_wrap_type, py_wrap_union_enum,
    pyo3::{
        pymethods,
        types::{PyFloat, PyInt},
        Py, PyResult, Python,
    },
    PyTryFrom, PyWrapper, PyWrapperMut, ToPython,
};

use super::PyMemoryReference;

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyArithmetic(Arithmetic) as "Arithmetic" {
        operator: ArithmeticOperator => PyArithmeticOperator,
        destination: ArithmeticOperand => PyArithmeticOperand,
        source: ArithmeticOperand => PyArithmeticOperand
    }
}
impl_repr!(PyArithmetic);

#[pymethods]
impl PyArithmetic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyArithmeticOperator,
        destination: PyArithmeticOperand,
        source: PyArithmeticOperand,
    ) -> PyResult<Self> {
        Ok(PyArithmetic(Arithmetic::new(
            ArithmeticOperator::py_try_from(py, &operator)?,
            ArithmeticOperand::py_try_from(py, &destination)?,
            ArithmeticOperand::py_try_from(py, &source)?,
        )))
    }
}

py_wrap_union_enum! {
    #[derive(Debug)]
    PyArithmeticOperand(ArithmeticOperand) as "ArithmeticOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        literal_real: LiteralReal => Py<PyFloat>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyArithmeticOperand);
impl_str!(PyArithmeticOperand);

py_wrap_simple_enum! {
    #[derive(Debug)]
    PyArithmeticOperator(ArithmeticOperator) as "ArithmeticOperator" {
        Add,
        Subtract,
        Divide,
        Multiply
    }
}
impl_repr!(PyArithmeticOperator);
impl_str!(PyArithmeticOperator);

py_wrap_union_enum! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyBinaryOperand(BinaryOperand) as "BinaryOperand" {
        literal_integer: LiteralInteger => Py<PyInt>,
        memory_reference: MemoryReference => PyMemoryReference
    }
}
impl_repr!(PyBinaryOperand);
impl_str!(PyBinaryOperand);

py_wrap_type! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyBinaryOperands(BinaryOperands) as "BinaryOperands"
}
impl_repr!(PyBinaryOperands);
impl_as_mut_for_wrapper!(PyBinaryOperands);

#[pymethods]
impl PyBinaryOperands {
    #[new]
    pub fn new(
        py: Python<'_>,
        memory_reference: PyMemoryReference,
        operand: PyBinaryOperand,
    ) -> PyResult<Self> {
        Ok(Self((
            MemoryReference::py_try_from(py, &memory_reference)?,
            BinaryOperand::py_try_from(py, &operand)?,
        )))
    }

    #[getter]
    pub fn get_memory_reference(&self, py: Python<'_>) -> PyResult<PyMemoryReference> {
        self.as_inner().0.to_python(py)
    }

    #[setter]
    pub fn set_memory_reference(
        &mut self,
        py: Python<'_>,
        memory_reference: PyMemoryReference,
    ) -> PyResult<()> {
        self.as_inner_mut().0 = MemoryReference::py_try_from(py, &memory_reference)?;
        Ok(())
    }

    #[getter]
    pub fn get_operand(&self, py: Python<'_>) -> PyResult<PyBinaryOperand> {
        self.as_inner().1.to_python(py)
    }

    #[setter]
    pub fn set_operand(&mut self, py: Python<'_>, binary_operand: PyBinaryOperand) -> PyResult<()> {
        self.as_inner_mut().1 = BinaryOperand::py_try_from(py, &binary_operand)?;
        Ok(())
    }
}

py_wrap_simple_enum! {
    #[derive(Debug)]
    PyBinaryOperator(BinaryOperator) as "BinaryOperator" {
        And,
        Ior,
        Xor
    }
}
impl_repr!(PyBinaryOperator);
impl_str!(PyBinaryOperator);

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyBinaryLogic(BinaryLogic) as "BinaryLogic" {
        operator: BinaryOperator => PyBinaryOperator,
        operands: BinaryOperands => PyBinaryOperands
    }
}
impl_repr!(PyBinaryLogic);

#[pymethods]
impl PyBinaryLogic {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyBinaryOperator,
        operands: PyBinaryOperands,
    ) -> PyResult<Self> {
        Ok(PyBinaryLogic(BinaryLogic::new(
            BinaryOperator::py_try_from(py, &operator)?,
            BinaryOperands::py_try_from(py, &operands)?,
        )))
    }
}
