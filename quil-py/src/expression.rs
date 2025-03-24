use std::collections::HashMap;

use pyo3::types::{PyList, PyTuple};
use quil_rs::expression::{
    Expression, ExpressionFunction, FunctionCallExpression, InfixExpression, InfixOperator,
    PrefixExpression, PrefixOperator, QuilFunction,
};

use rigetti_pyo3::{
    create_init_submodule, impl_from_str, impl_hash, impl_parse, impl_repr, impl_str,
    num_complex::Complex64,
    py_wrap_data_struct, py_wrap_error, py_wrap_simple_enum, py_wrap_union_enum,
    pyo3::{
        exceptions::PyValueError,
        pymethods,
        types::{PyComplex, PyString},
        Py, PyResult, Python,
    },
    wrap_error, PyTryFrom, PyWrapper, PyWrapperMut, ToPython, ToPythonError,
};

use internment::ArcIntern;

use crate::{impl_eq, impl_to_quil, instruction::PyMemoryReference};

wrap_error!(RustEvaluationError(quil_rs::expression::EvaluationError));
py_wrap_error!(quil, RustEvaluationError, EvaluationError, PyValueError);
wrap_error!(RustParseExpressionError(quil_rs::program::ParseProgramError<Expression>));
py_wrap_error!(
    quil,
    RustParseExpressionError,
    ParseExpressionError,
    PyValueError
);

py_wrap_union_enum! {
    #[derive(Debug, Hash, PartialEq, Eq)]
    #[pyo3(module="quil.expression")]
    PyExpression(Expression) as "Expression" {
        address: Address => PyMemoryReference,
        function_call: FunctionCall => PyFunctionCallExpression,
        infix: Infix => PyInfixExpression,
        number: Number => Py<PyComplex>,
        pi: PiConstant,
        prefix: Prefix => PyPrefixExpression,
        variable: Variable => Py<PyString>
    }
}
impl_repr!(PyExpression);
impl_to_quil!(PyExpression);
impl_from_str!(PyExpression, RustParseExpressionError);
impl_hash!(PyExpression);
impl_parse!(PyExpression);
impl_eq!(PyExpression);

#[pymethods]
impl PyExpression {
    pub fn simplify(&mut self) {
        self.as_inner_mut().simplify()
    }

    pub fn into_simplified(&self, py: Python<'_>) -> PyResult<Self> {
        self.as_inner().clone().into_simplified().to_python(py)
    }

    pub fn evaluate(
        &self,
        variables: HashMap<String, Complex64>,
        memory_references: HashMap<&str, Vec<f64>>,
    ) -> PyResult<Complex64> {
        self.as_inner()
            .evaluate(&variables, &memory_references)
            .map_err(RustEvaluationError::from)
            .map_err(RustEvaluationError::to_py_err)
    }

    pub fn substitute_variables(
        &self,
        py: Python<'_>,
        variable_values: HashMap<String, PyExpression>,
    ) -> PyResult<Self> {
        Ok(PyExpression(self.as_inner().clone().substitute_variables(
            &HashMap::<String, Expression>::py_try_from(py, &variable_values)?,
        )))
    }

    pub fn to_real(&self) -> PyResult<f64> {
        self.as_inner()
            .to_real()
            .map_err(RustEvaluationError::from)
            .map_err(RustEvaluationError::to_py_err)
    }

    pub fn __add__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() + other.as_inner().clone())
    }

    pub fn __sub__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() - other.as_inner().clone())
    }

    pub fn __mul__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() * other.as_inner().clone())
    }

    pub fn __truediv__(&self, other: PyExpression) -> Self {
        PyExpression(self.as_inner().clone() / other.as_inner().clone())
    }
}

py_wrap_data_struct! {
    #[pyo3(subclass)]
    #[derive(Debug)]
    PyFunctionCallExpression(FunctionCallExpression) as "FunctionCallExpression" {
        function: ExpressionFunction => PyExpressionFunction,
        arguments: Vec<ArcIntern<Expression>> => Vec<PyExpression> => Py<PyList>
    }
}
impl_repr!(PyFunctionCallExpression);

#[pymethods]
impl PyFunctionCallExpression {
    #[new]
    #[pyo3(signature = (function, *arguments))]
    pub fn new(
        py: Python<'_>,
        function: PyExpressionFunction,
        arguments: &PyTuple,
    ) -> PyResult<Self> {
        Ok(PyFunctionCallExpression(FunctionCallExpression::new(
            ExpressionFunction::py_try_from(py, &function)?,
            arguments
                .iter()
                .map(|arg| ArcIntern::<Expression>::py_try_from(py, &arg.extract()?))
                .collect::<PyResult<_>>()?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyInfixExpression(InfixExpression) as "InfixExpression" {
        left: ArcIntern<Expression> => PyExpression,
        operator: InfixOperator => PyInfixOperator,
        right: ArcIntern<Expression> => PyExpression
    }
}
impl_repr!(PyInfixExpression);

#[pymethods]
impl PyInfixExpression {
    #[new]
    pub fn new(
        py: Python<'_>,
        left: PyExpression,
        operator: PyInfixOperator,
        right: PyExpression,
    ) -> PyResult<Self> {
        Ok(PyInfixExpression(InfixExpression::new(
            ArcIntern::<Expression>::py_try_from(py, &left)?,
            InfixOperator::py_try_from(py, &operator)?,
            ArcIntern::<Expression>::py_try_from(py, &right)?,
        )))
    }
}

py_wrap_data_struct! {
    #[derive(Debug)]
    #[pyo3(subclass)]
    PyPrefixExpression(PrefixExpression) as "PrefixExpression" {
        operator: PrefixOperator => PyPrefixOperator,
        expression: ArcIntern<Expression> => PyExpression
    }
}

#[pymethods]
impl PyPrefixExpression {
    #[new]
    pub fn new(
        py: Python<'_>,
        operator: PyPrefixOperator,
        expression: PyExpression,
    ) -> PyResult<Self> {
        Ok(PyPrefixExpression(PrefixExpression::new(
            PrefixOperator::py_try_from(py, &operator)?,
            ArcIntern::<Expression>::py_try_from(py, &expression)?,
        )))
    }
}

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyQuilFunction(QuilFunction) as "QuilFunction" {
        Cis,
        Cosine,
        Exponent,
        Sine,
        SquareRoot
    }
}
impl_repr!(PyQuilFunction);
impl_str!(PyQuilFunction);
impl_hash!(PyQuilFunction);
impl_eq!(PyQuilFunction);

// Expanded from
//
// py_wrap_union_enum! {
//     #[derive(Debug, Hash, PartialEq, Eq)]
//     PyExpressionFunction(ExpressionFunction) as "ExpressionFunction" {
//         builtin: Builtin => QuilFunction,
//         extern: Extern => Py<PyString>
//     }
// }
//
// Which doesn't work due to trait impl issues with QuilFunction.  Changes in the expansion are
// highlighted with "CHANGED".

#[repr(transparent)]
#[rigetti_pyo3::pyo3::pyclass(name = "ExpressionFunction")]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PyExpressionFunction(ExpressionFunction);

#[allow(clippy::needless_lifetimes, clippy::match_like_matches_macro)]
mod expanded {
    use super::*;

    impl rigetti_pyo3::PyTryFrom<PyExpressionFunction> for ExpressionFunction {
        fn py_try_from(
            _py: rigetti_pyo3::pyo3::Python,
            item: &PyExpressionFunction,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            Ok(item.0.clone())
        }
    }
    impl rigetti_pyo3::PyTryFrom<rigetti_pyo3::pyo3::PyAny> for PyExpressionFunction {
        fn py_try_from(
            _py: rigetti_pyo3::pyo3::Python,
            item: &rigetti_pyo3::pyo3::PyAny,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            item.extract()
        }
    }
    impl rigetti_pyo3::PyTryFrom<PyExpressionFunction> for PyExpressionFunction {
        fn py_try_from(
            _py: rigetti_pyo3::pyo3::Python,
            item: &PyExpressionFunction,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            Ok(item.clone())
        }
    }
    impl rigetti_pyo3::ToPython<PyExpressionFunction> for ExpressionFunction {
        fn to_python(
            &self,
            _py: rigetti_pyo3::pyo3::Python<'_>,
        ) -> rigetti_pyo3::pyo3::PyResult<PyExpressionFunction> {
            {
                Ok(PyExpressionFunction::from(self.clone()))
            }
        }
    }
    impl<'a> rigetti_pyo3::ToPython<PyExpressionFunction> for &'a ExpressionFunction {
        fn to_python(
            &self,
            py: rigetti_pyo3::pyo3::Python<'_>,
        ) -> rigetti_pyo3::pyo3::PyResult<PyExpressionFunction> {
            {
                <ExpressionFunction as rigetti_pyo3::ToPython<PyExpressionFunction>>::to_python(
                    *self, py,
                )
            }
        }
    }
    #[allow(clippy::use_self)]
    impl rigetti_pyo3::ToPython<rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>>
        for PyExpressionFunction
    {
        fn to_python(
            &self,
            py: rigetti_pyo3::pyo3::Python<'_>,
        ) -> rigetti_pyo3::pyo3::PyResult<rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>>
        {
            {
                Ok(<Self as rigetti_pyo3::pyo3::ToPyObject>::to_object(
                    self, py,
                ))
            }
        }
    }
    #[allow(clippy::use_self)]
    impl<'a> rigetti_pyo3::ToPython<rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>>
        for &'a PyExpressionFunction
    {
        fn to_python(
            &self,
            py: rigetti_pyo3::pyo3::Python<'_>,
        ) -> rigetti_pyo3::pyo3::PyResult<rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>>
        {
            {
                <PyExpressionFunction as rigetti_pyo3::ToPython<
                    rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>,
                >>::to_python(*self, py)
            }
        }
    }
    impl From<PyExpressionFunction> for ExpressionFunction {
        fn from(wrapper: PyExpressionFunction) -> Self {
            wrapper.0
        }
    }
    impl From<ExpressionFunction> for PyExpressionFunction {
        fn from(inner: ExpressionFunction) -> Self {
            Self(inner)
        }
    }
    impl From<&ExpressionFunction> for PyExpressionFunction {
        fn from(inner: &ExpressionFunction) -> Self {
            Self(inner.clone())
        }
    }
    impl AsRef<ExpressionFunction> for PyExpressionFunction {
        fn as_ref(&self) -> &ExpressionFunction {
            &self.0
        }
    }
    impl rigetti_pyo3::PyWrapper for PyExpressionFunction {
        type Inner = ExpressionFunction;
    }
    impl rigetti_pyo3::pyo3::conversion::ToPyObject for PyExpressionFunction {
        fn to_object(&self, py: rigetti_pyo3::pyo3::Python) -> rigetti_pyo3::pyo3::PyObject {
            #[allow(clippy::use_self)]
            const NAME: &str = stringify!(PyExpressionFunction);
            let cell = rigetti_pyo3::pyo3::PyCell::new(py, self.clone())
                .unwrap_or_else(|err| panic!("failed to create {} on Python heap: {}", NAME, err));
            rigetti_pyo3::pyo3::conversion::ToPyObject::to_object(&cell, py)
        }
    }
    impl AsMut<<PyExpressionFunction as rigetti_pyo3::PyWrapper>::Inner> for PyExpressionFunction {
        fn as_mut(&mut self) -> &mut <PyExpressionFunction as rigetti_pyo3::PyWrapper>::Inner {
            &mut self.0
        }
    }
    #[rigetti_pyo3::pyo3::pymethods]
    impl PyExpressionFunction {
        #[doc = concat!(r"The Python wrapper for [`",stringify!(ExpressionFunction),r"::",stringify!(Builtin),r"`], creating a [`",stringify!(PyExpressionFunction),r"`] and taking a Python argument.")]
        #[staticmethod]
        pub fn from_builtin(
            py: rigetti_pyo3::pyo3::Python,
            inner: Py<PyQuilFunction>,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            // CHANGED
            Ok(Self(ExpressionFunction::Builtin(
                *inner.into_ref(py).try_borrow()?.as_ref(),
            )))
        }
    }
    #[rigetti_pyo3::pyo3::pymethods]
    impl PyExpressionFunction {
        #[doc = concat!(r"The Python wrapper for [`",stringify!(ExpressionFunction),r"::",stringify!(Extern),r"`], creating a [`",stringify!(PyExpressionFunction),r"`] and taking a Python argument.")]
        #[staticmethod]
        pub fn from_extern(
            py: rigetti_pyo3::pyo3::Python,
            inner: Py<PyString>,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            let inner = &inner;
            { <_ as rigetti_pyo3::PyTryFrom<Py<PyString>>>::py_try_from(py, inner) }
                .map(ExpressionFunction::Extern)
                .map(Self)
        }
    }
    #[rigetti_pyo3::pyo3::pymethods]
    impl PyExpressionFunction {
        #[doc = concat!(r"Create a new [`",stringify!(PyExpressionFunction),r"`] from a Python argument; corresponds to `","ExpressionFunction",r".",r"__new__()` in Python")]
        #[new]
        pub fn new(
            py: rigetti_pyo3::pyo3::Python,
            input: &rigetti_pyo3::pyo3::PyAny,
        ) -> rigetti_pyo3::pyo3::PyResult<Self> {
            if let Ok(inner) = <Py<PyQuilFunction> as rigetti_pyo3::PyTryFrom<
                rigetti_pyo3::pyo3::PyAny,
            >>::py_try_from(py, input)
            {
                // CHANGED
                let converted = inner.into_ref(py).try_borrow().map(|x| *x.as_ref());
                if let Ok(item) = converted {
                    return Ok(Self::from(ExpressionFunction::Builtin(item)));
                }
            }
            if let Ok(inner) =
                <_ as rigetti_pyo3::PyTryFrom<rigetti_pyo3::pyo3::PyAny>>::py_try_from(py, input)
            {
                let inner = &inner;
                let converted =
                    { <_ as rigetti_pyo3::PyTryFrom<Py<PyString>>>::py_try_from(py, inner) };
                if let Ok(item) = converted {
                    return Ok(Self::from(ExpressionFunction::Extern(item)));
                }
            }
            Err(rigetti_pyo3::pyo3::exceptions::PyValueError::new_err(
                format!(
                    "could not create {} from {}",
                    stringify!(PyExpressionFunction),
                    input.repr()?
                ),
            ))
        }
        #[doc = concat!(r"Directly return the Python version of the variant discriminant wrapped by this ",r"value; ",r"i.e., performs the match `",stringify!(ExpressionFunction),r"::Variant(x) => x` for every variant constructor in [`",stringify!(ExpressionFunction),r"`]")]
        pub fn inner(
            &self,
            py: rigetti_pyo3::pyo3::Python,
        ) -> rigetti_pyo3::pyo3::PyResult<rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>>
        {
            match &self.0 {
                ExpressionFunction::Builtin(inner) => Ok(rigetti_pyo3::pyo3::conversion::IntoPy::<
                    rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>,
                >::into_py(
                    {
                        // CHANGED – added Py::new
                        let inner: Py<PyQuilFunction> = Py::new(
                            py,
                            rigetti_pyo3::ToPython::<PyQuilFunction>::to_python(&inner, py)?,
                        )?;
                        Ok::<_, rigetti_pyo3::pyo3::PyErr>(inner)
                    }?,
                    py,
                )),
                ExpressionFunction::Extern(inner) => Ok(rigetti_pyo3::pyo3::conversion::IntoPy::<
                    rigetti_pyo3::pyo3::Py<rigetti_pyo3::pyo3::PyAny>,
                >::into_py(
                    {
                        let inner: Py<PyString> =
                            rigetti_pyo3::ToPython::<Py<PyString>>::to_python(&inner, py)?;
                        Ok::<_, rigetti_pyo3::pyo3::PyErr>(inner)
                    }?,
                    py,
                )),
            }
        }
        #[doc = concat!(r"Tests if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a [`",stringify!(ExpressionFunction),r"::",stringify!(builtin),"`] value")]
        const fn is_builtin(&self) -> bool {
            match &self.0 {
                ExpressionFunction::Builtin(_) => true,
                _ => false,
            }
        }
        #[doc = concat!(r"Returns `x` if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a `",stringify!(ExpressionFunction),r"::",stringify!(builtin),"`(x); ",r"otherwise returns (Python) `None`.  On the Rust side, this corresponds to ",r"either `Some(x)` or [`None`].")]
        fn as_builtin(&self, py: rigetti_pyo3::pyo3::Python) -> Option<Py<PyQuilFunction>> {
            self.to_builtin(py).ok()
        }
        #[doc = concat!(r"Returns `x` if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a `",stringify!(ExpressionFunction),r"::",stringify!(builtin),"`(x); ",r"otherwise raises a `ValueError`.  On the Rust side, this corresponds to ",r"either `Ok(x)` or `Err(...)`.")]
        fn to_builtin(
            &self,
            py: rigetti_pyo3::pyo3::Python,
        ) -> rigetti_pyo3::pyo3::PyResult<Py<PyQuilFunction>> {
            if let ExpressionFunction::Builtin(inner) = &self.0 {
                {
                    // CHANGED - added Py::new
                    let inner: Py<PyQuilFunction> = Py::new(
                        py,
                        rigetti_pyo3::ToPython::<PyQuilFunction>::to_python(&inner, py)?,
                    )?;
                    Ok::<_, rigetti_pyo3::pyo3::PyErr>(inner)
                }
            } else {
                Err(rigetti_pyo3::pyo3::exceptions::PyValueError::new_err(
                    concat!("expected self to be a ", stringify!(builtin)),
                ))
            }
        }
        #[doc = concat!(r"Tests if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a [`",stringify!(ExpressionFunction),r"::",stringify!(extern),"`] value")]
        const fn is_extern(&self) -> bool {
            match &self.0 {
                ExpressionFunction::Extern(_) => true,
                _ => false,
            }
        }
        #[doc = concat!(r"Returns `x` if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a `",stringify!(ExpressionFunction),r"::",stringify!(extern),"`(x); ",r"otherwise returns (Python) `None`.  On the Rust side, this corresponds to ",r"either `Some(x)` or [`None`].")]
        fn as_extern(&self, py: rigetti_pyo3::pyo3::Python) -> Option<Py<PyString>> {
            self.to_extern(py).ok()
        }
        #[doc = concat!(r"Returns `x` if this [`",stringify!(PyExpressionFunction),r"`] ",r"wraps a `",stringify!(ExpressionFunction),r"::",stringify!(extern),"`(x); ",r"otherwise raises a `ValueError`.  On the Rust side, this corresponds to ",r"either `Ok(x)` or `Err(...)`.")]
        fn to_extern(
            &self,
            py: rigetti_pyo3::pyo3::Python,
        ) -> rigetti_pyo3::pyo3::PyResult<Py<PyString>> {
            if let ExpressionFunction::Extern(inner) = &self.0 {
                {
                    let inner: Py<PyString> =
                        rigetti_pyo3::ToPython::<Py<PyString>>::to_python(&inner, py)?;
                    Ok::<_, rigetti_pyo3::pyo3::PyErr>(inner)
                }
            } else {
                Err(rigetti_pyo3::pyo3::exceptions::PyValueError::new_err(
                    concat!("expected self to be a ", stringify!(extern)),
                ))
            }
        }
    }
}
impl_repr!(PyExpressionFunction);
impl_str!(PyExpressionFunction);
impl_hash!(PyExpressionFunction);
impl_eq!(PyExpressionFunction);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyPrefixOperator(PrefixOperator) as "PrefixOperator" {
        Plus,
        Minus
    }
}
impl_repr!(PyPrefixOperator);
impl_str!(PyPrefixOperator);
impl_hash!(PyPrefixOperator);
impl_eq!(PyPrefixOperator);

py_wrap_simple_enum! {
    #[derive(Debug, PartialEq, Eq, Hash)]
    PyInfixOperator(InfixOperator) as "InfixOperator" {
        Caret,
        Plus,
        Minus,
        Slash,
        Star
    }
}
impl_repr!(PyInfixOperator);
impl_str!(PyInfixOperator);
impl_hash!(PyInfixOperator);
impl_eq!(PyInfixOperator);

create_init_submodule! {
    classes: [
        PyExpression,
        PyFunctionCallExpression,
        PyInfixExpression,
        PyPrefixExpression,
        PyExpressionFunction,
        PyQuilFunction,
        PyPrefixOperator,
        PyInfixOperator
    ],
    errors: [
        EvaluationError,
        ParseExpressionError
    ],
}
