/// This module provides support for the `CALL` instruction and the reserved `PRAGMA EXTERN` instruction.
///
/// For additional detail on its design and specification see:
///
/// * [Quil specification "Other"](https://github.com/quil-lang/quil/blob/7f532c7cdde9f51eae6abe7408cc868fba9f91f6/specgen/spec/sec-other.s_)
/// * [Quil EXTERN / CALL RFC](https://github.com/quil-lang/quil/blob/master/rfcs/extern-call.md)
/// * [quil#69](https://github.com/quil-lang/quil/pull/69)
use std::{collections::HashSet, str::FromStr};

use indexmap::IndexMap;
use nom_locate::LocatedSpan;
use num_complex::Complex64;

use crate::{
    expression::format_complex,
    hash::hash_f64,
    parser::lex,
    program::{disallow_leftover, MemoryAccesses, MemoryRegion, SyntaxError},
    quil::Quil,
    validation::identifier::{validate_user_identifier, IdentifierValidationError},
};

use super::{
    Instruction, MemoryReference, Pragma, PragmaArgument, ScalarType, Vector,
    RESERVED_PRAGMA_EXTERN,
};

/// A parameter type within an extern signature.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum ExternParameterType {
    /// A scalar parameter, which may accept a memory reference or immediate value.
    ///
    /// For instance `PRAGMA EXTERN foo "(bar : INTEGER)"`.
    Scalar(ScalarType),
    /// A fixed-length vector, which must accept a memory region name of the appropriate
    /// length and data type.
    ///
    /// For instance `PRAGMA EXTERN foo "(bar : INTEGER[2])"`.
    FixedLengthVector(Vector),
    /// A variable-length vector, which must accept a memory region name of the appropriate
    /// data type.
    ///
    /// For instance `PRAGMA EXTERN foo "(bar : INTEGER[])"`.
    VariableLengthVector(ScalarType),
}

impl Quil for ExternParameterType {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            ExternParameterType::Scalar(value) => value.write(f, fall_back_to_debug),
            ExternParameterType::FixedLengthVector(value) => value.write(f, fall_back_to_debug),
            ExternParameterType::VariableLengthVector(value) => {
                value.write(f, fall_back_to_debug)?;
                Ok(write!(f, "[]")?)
            }
        }
    }
}

/// An extern parameter with a name, mutability, and data type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExternParameter {
    /// The name of the parameter. This must be a valid user identifier.
    pub(crate) name: String,
    /// Whether the parameter is mutable.
    pub(crate) mutable: bool,
    /// The data type of the parameter.
    pub(crate) data_type: ExternParameterType,
}

impl ExternParameter {
    /// Create a new extern parameter. This will fail if the parameter name
    /// is not a valid user identifier.
    pub fn try_new(
        name: String,
        mutable: bool,
        data_type: ExternParameterType,
    ) -> Result<Self, ExternError> {
        validate_user_identifier(name.as_str()).map_err(ExternError::from)?;
        Ok(Self {
            name,
            mutable,
            data_type,
        })
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }

    pub fn data_type(&self) -> &ExternParameterType {
        &self.data_type
    }
}

impl Quil for ExternParameter {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "{} : ", self.name)?;
        if self.mutable {
            write!(writer, "mut ")?;
        }
        self.data_type.write(writer, fall_back_to_debug)
    }
}

/// An extern signature with a return type and parameters.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExternSignature {
    /// The return type of the extern signature, if any.
    pub(crate) return_type: Option<ScalarType>,
    /// The parameters of the extern signature.
    pub(crate) parameters: Vec<ExternParameter>,
}

impl ExternSignature {
    /// Create a new extern signature.
    pub fn new(return_type: Option<ScalarType>, parameters: Vec<ExternParameter>) -> Self {
        Self {
            return_type,
            parameters,
        }
    }

    pub fn return_type(&self) -> Option<&ScalarType> {
        self.return_type.as_ref()
    }

    pub fn parameters(&self) -> &[ExternParameter] {
        self.parameters.as_slice()
    }
}

const EXPECTED_PRAGMA_EXTERN_STRUCTURE: &str = "PRAGMA EXTERN {name} \"{scalar type}? (\\(({parameter name} : mut? {parameter type}) (, {parameter name} : mut? {parameter type})*\\))?\"";

/// An error that can occur when parsing an extern signature.
#[derive(Debug, thiserror::Error, PartialEq, Clone)]
pub enum ExternError {
    /// An error occurred while parsing the contents of the extern signature.
    #[error(
        "invalid extern signature syntax: {0:?} (expected `{EXPECTED_PRAGMA_EXTERN_STRUCTURE}`)"
    )]
    Syntax(SyntaxError<ExternSignature>),
    /// An error occurred while lexing the extern signature.
    #[error(
        "failed to lex extern signature: {0:?} (expected `{EXPECTED_PRAGMA_EXTERN_STRUCTURE}`)"
    )]
    Lex(crate::parser::LexError),
    /// Pragma arguments are invalid.
    #[error("`PRAGMA EXTERN` must have a single argument representing the extern name")]
    InvalidPragmaArguments,
    /// No signature found.
    #[error("`PRAGMA EXTERN` instruction has no signature")]
    NoSignature,
    /// No extern name found.
    #[error("`PRAGMA EXTERN` instruction has no name")]
    NoName,
    /// Pragma is not EXTERN.
    #[error("ExternPragmaMap contained a pragma that was not EXTERN")]
    PragmaIsNotExtern,
    /// The extern definition has a signature but it has neither a return nor parameters.
    #[error("extern definition has a signature but it has neither a return nor parameters")]
    NoReturnOrParameters,
    /// Either the name of the extern or one of its parameters is invalid.
    #[error("invalid identifier: {0:?}")]
    Name(#[from] IdentifierValidationError),
}

impl FromStr for ExternSignature {
    type Err = ExternError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let signature_input = LocatedSpan::new(s);
        let signature_tokens = lex(signature_input).map_err(ExternError::Lex)?;
        let signature = disallow_leftover(
            crate::parser::pragma_extern::parse_extern_signature(signature_tokens.as_slice())
                .map_err(crate::parser::ParseError::from_nom_internal_err),
        )
        .map_err(ExternError::Syntax)?;
        if signature.return_type.is_none() && signature.parameters.is_empty() {
            return Err(ExternError::NoReturnOrParameters);
        }
        for parameter in &signature.parameters {
            validate_user_identifier(parameter.name.as_str()).map_err(ExternError::from)?;
        }
        Ok(signature)
    }
}

impl Quil for ExternSignature {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        if let Some(return_type) = &self.return_type {
            return_type.write(writer, fall_back_to_debug)?;
            if !self.parameters.is_empty() {
                write!(writer, " ")?;
            }
        }
        if self.parameters.is_empty() {
            return Ok(());
        }
        write!(writer, "(")?;
        for (i, parameter) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(writer, ", ")?;
            }
            parameter.write(writer, fall_back_to_debug)?;
        }
        write!(writer, ")").map_err(Into::into)
    }
}

impl TryFrom<Pragma> for ExternSignature {
    type Error = ExternError;

    fn try_from(value: Pragma) -> Result<Self, ExternError> {
        if value.name != RESERVED_PRAGMA_EXTERN {
            return Err(ExternError::PragmaIsNotExtern);
        }
        if value.arguments.is_empty()
            || !matches!(value.arguments[0], PragmaArgument::Identifier(_))
        {
            return Err(ExternError::NoName);
        }
        if value.arguments.len() > 1 {
            return Err(ExternError::InvalidPragmaArguments);
        }

        match value.data {
            Some(data) => ExternSignature::from_str(data.as_str()),
            None => Err(ExternError::NoSignature),
        }
    }
}

/// A map of all program `PRAGMA EXTERN` instructions from their name (if any) to
/// the corresponding [`Pragma`] instruction. Note, keys are [`Option`]s, but a
/// `None` key will be considered invalid when converting to an [`ExternSignatureMap`].
#[derive(Clone, Debug, PartialEq, Default)]
pub struct ExternPragmaMap(IndexMap<Option<String>, Pragma>);

impl ExternPragmaMap {
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn to_instructions(&self) -> Vec<Instruction> {
        self.0.values().cloned().map(Instruction::Pragma).collect()
    }

    /// Insert a `PRAGMA EXTERN` instruction into the underlying [`IndexMap`].
    ///
    /// If the first argument to the [`Pragma`] is not a [`PragmaArgument::Identifier`], or
    /// does not exist, then the [`Pragma`] will be inserted with a `None` key.
    ///
    /// If the key already exists, the previous [`Pragma`] will be returned, similar to
    /// the behavior of [`IndexMap::insert`].
    pub(crate) fn insert(&mut self, pragma: Pragma) -> Option<Pragma> {
        self.0.insert(
            match pragma.arguments.first() {
                Some(PragmaArgument::Identifier(name)) => Some(name.clone()),
                _ => None,
            },
            pragma,
        )
    }

    pub(crate) fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Option<String>, &mut Pragma) -> bool,
    {
        self.0.retain(f)
    }
}

/// A map of all program `PRAGMA EXTERN` instructions from their name to the corresponding
/// parsed and validated [`ExternSignature`].
#[derive(Clone, Debug, PartialEq, Default)]
pub struct ExternSignatureMap(IndexMap<String, ExternSignature>);

impl TryFrom<ExternPragmaMap> for ExternSignatureMap {
    /// The error type for converting an [`ExternPragmaMap`] to an [`ExternSignatureMap`] includes
    /// the offending [`Pragma`] instruction and the error that occurred.
    type Error = (Pragma, ExternError);

    fn try_from(value: ExternPragmaMap) -> Result<Self, Self::Error> {
        Ok(ExternSignatureMap(
            value
                .0
                .into_iter()
                .map(|(key, value)| -> Result<_, Self::Error> {
                    match key {
                        Some(name) => {
                            validate_user_identifier(name.as_str())
                                .map_err(ExternError::from)
                                .map_err(|error| (value.clone(), error))?;
                            let signature = ExternSignature::try_from(value.clone())
                                .map_err(|error| (value, error))?;
                            Ok((name, signature))
                        }
                        _ => Err((value, ExternError::NoName)),
                    }
                })
                .collect::<Result<_, Self::Error>>()?,
        ))
    }
}

impl ExternSignatureMap {
    #[cfg(test)]
    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }
}

/// An error that can occur when resolving a call instruction.
#[derive(Clone, Debug, thiserror::Error, PartialEq)]
pub enum CallArgumentResolutionError {
    /// An undeclared memory reference was encountered.
    #[error("undeclared memory reference {0}")]
    UndeclaredMemoryReference(String),
    /// A mismatched vector was encountered.
    #[error("mismatched vector: expected {expected:?}, found {found:?}")]
    MismatchedVector { expected: Vector, found: Vector },
    /// A mismatched scalar was encountered.
    #[error("mismatched scalar: expected {expected:?}, found {found:?}")]
    MismatchedScalar {
        expected: ScalarType,
        found: ScalarType,
    },
    /// The argument for a vector parameter was invalid.
    #[error("vector parameters must be passed as an identifier, found {0:?}")]
    InvalidVectorArgument(UnresolvedCallArgument),
    /// The argument for a return parameter was invalid.
    #[error("return argument must be a memory reference or identifier, found {found:?}")]
    ReturnArgument { found: UnresolvedCallArgument },
    /// Immediate arguments cannot be specified for mutable parameters.
    #[error("immediate arguments cannot be specified for mutable parameter {0}")]
    ImmediateArgumentForMutable(String),
}

/// A parsed, but unresolved call argument. This may be resolved into a [`ResolvedCallArgument`]
/// with the appropriate [`ExternSignature`]. Resolution is required for building the
/// [`crate::Program`] memory graph.
#[derive(Clone, Debug, PartialEq)]
pub enum UnresolvedCallArgument {
    /// A reference to a declared memory location. Note, this may be resolved to either
    /// a scalar or vector. In the former case, the assumed index is 0.
    Identifier(String),
    /// A reference to a memory location. This may be resolved to a scalar.
    MemoryReference(MemoryReference),
    /// An immediate value. This may be resolved to a non-mutable scalar.
    Immediate(Complex64),
}

impl Eq for UnresolvedCallArgument {}

impl std::hash::Hash for UnresolvedCallArgument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            UnresolvedCallArgument::Identifier(value) => {
                "Identifier".hash(state);
                value.hash(state);
            }
            UnresolvedCallArgument::MemoryReference(value) => {
                "MemoryReference".hash(state);
                value.hash(state);
            }
            UnresolvedCallArgument::Immediate(value) => {
                "Immediate".hash(state);
                hash_complex_64(value, state);
            }
        }
    }
}

impl UnresolvedCallArgument {
    /// Check if the argument is compatible with the given [`ExternParameter`]. If so, return
    /// the appropriate [`ResolvedCallArgument`]. If not, return an error.
    fn resolve(
        &self,
        memory_regions: &IndexMap<String, MemoryRegion>,
        extern_parameter: &ExternParameter,
    ) -> Result<ResolvedCallArgument, CallArgumentResolutionError> {
        match self {
            UnresolvedCallArgument::Identifier(value) => {
                let expected_vector = match &extern_parameter.data_type {
                    ExternParameterType::Scalar(_) => {
                        return UnresolvedCallArgument::MemoryReference(MemoryReference::new(
                            value.clone(),
                            0,
                        ))
                        .resolve(memory_regions, extern_parameter);
                    }
                    ExternParameterType::FixedLengthVector(expected_vector) => {
                        let memory_region =
                            memory_regions.get(value.as_str()).ok_or_else(|| {
                                CallArgumentResolutionError::UndeclaredMemoryReference(
                                    value.clone(),
                                )
                            })?;
                        if &memory_region.size != expected_vector {
                            return Err(CallArgumentResolutionError::MismatchedVector {
                                expected: expected_vector.clone(),
                                found: memory_region.size.clone(),
                            });
                        }

                        Ok(expected_vector.clone())
                    }
                    ExternParameterType::VariableLengthVector(scalar_type) => {
                        let memory_region =
                            memory_regions.get(value.as_str()).ok_or_else(|| {
                                CallArgumentResolutionError::UndeclaredMemoryReference(
                                    value.clone(),
                                )
                            })?;
                        if &memory_region.size.data_type != scalar_type {
                            return Err(CallArgumentResolutionError::MismatchedScalar {
                                expected: *scalar_type,
                                found: memory_region.size.data_type,
                            });
                        }
                        Ok(memory_region.size.clone())
                    }
                }?;
                Ok(ResolvedCallArgument::Vector {
                    memory_region_name: value.clone(),
                    vector: expected_vector,
                    mutable: extern_parameter.mutable,
                })
            }
            UnresolvedCallArgument::MemoryReference(value) => {
                let expected_scalar = match extern_parameter.data_type {
                    ExternParameterType::Scalar(ref scalar) => Ok(scalar),
                    ExternParameterType::FixedLengthVector(_)
                    | ExternParameterType::VariableLengthVector(_) => {
                        Err(CallArgumentResolutionError::InvalidVectorArgument(
                            Self::MemoryReference(value.clone()),
                        ))
                    }
                }?;
                let memory_region = memory_regions.get(value.name.as_str()).ok_or_else(|| {
                    CallArgumentResolutionError::UndeclaredMemoryReference(value.name.clone())
                })?;
                if memory_region.size.data_type != *expected_scalar {
                    return Err(CallArgumentResolutionError::MismatchedScalar {
                        expected: *expected_scalar,
                        found: memory_region.size.data_type,
                    });
                }
                Ok(ResolvedCallArgument::MemoryReference {
                    memory_reference: value.clone(),
                    scalar_type: *expected_scalar,
                    mutable: extern_parameter.mutable,
                })
            }
            UnresolvedCallArgument::Immediate(value) => {
                if extern_parameter.mutable {
                    return Err(CallArgumentResolutionError::ImmediateArgumentForMutable(
                        extern_parameter.name.clone(),
                    ));
                }
                let expected_scalar = match extern_parameter.data_type {
                    ExternParameterType::Scalar(ref scalar) => Ok(scalar),
                    ExternParameterType::FixedLengthVector(_)
                    | ExternParameterType::VariableLengthVector(_) => Err(
                        CallArgumentResolutionError::InvalidVectorArgument(self.clone()),
                    ),
                }?;
                Ok(ResolvedCallArgument::Immediate {
                    value: *value,
                    scalar_type: *expected_scalar,
                })
            }
        }
    }

    /// Check if the argument is compatible with the return type of the [`ExternSignature`]. If so,
    /// return the appropriate [`ResolvedCallArgument`]. If not, return an error.
    fn resolve_return(
        &self,
        memory_regions: &IndexMap<String, MemoryRegion>,
        return_type: ScalarType,
    ) -> Result<ResolvedCallArgument, CallArgumentResolutionError> {
        let memory_reference = match self {
            UnresolvedCallArgument::MemoryReference(memory_reference) => {
                Ok(memory_reference.clone())
            }
            UnresolvedCallArgument::Identifier(identifier) => {
                Ok(MemoryReference::new(identifier.clone(), 0))
            }
            _ => Err(CallArgumentResolutionError::ReturnArgument {
                found: self.clone(),
            }),
        }?;
        let memory_region = memory_regions
            .get(memory_reference.name.as_str())
            .ok_or_else(|| {
                CallArgumentResolutionError::UndeclaredMemoryReference(
                    memory_reference.name.clone(),
                )
            })?;
        if memory_region.size.data_type != return_type {
            return Err(CallArgumentResolutionError::MismatchedScalar {
                expected: return_type,
                found: memory_region.size.data_type,
            });
        }
        Ok(ResolvedCallArgument::MemoryReference {
            memory_reference: memory_reference.clone(),
            scalar_type: return_type,
            mutable: true,
        })
    }
}

impl Quil for UnresolvedCallArgument {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match &self {
            UnresolvedCallArgument::Identifier(value) => write!(f, "{value}",).map_err(Into::into),
            UnresolvedCallArgument::MemoryReference(value) => value.write(f, fall_back_to_debug),
            UnresolvedCallArgument::Immediate(value) => {
                write!(f, "{}", format_complex(value)).map_err(Into::into)
            }
        }
    }
}

/// A resolved call argument. This is the result of resolving an [`UnresolvedCallArgument`] with
/// the appropriate [`ExternParameter`]. It annotates the argument both with a type (and possibly
/// a length in the case of a vector) and mutability.
#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedCallArgument {
    /// A resolved vector argument, including its scalar type, length, and mutability.
    Vector {
        memory_region_name: String,
        vector: Vector,
        mutable: bool,
    },
    /// A resolved memory reference, including its scalar type and mutability.
    MemoryReference {
        memory_reference: MemoryReference,
        scalar_type: ScalarType,
        mutable: bool,
    },
    /// A resolved immediate value, including its scalar type.
    Immediate {
        value: Complex64,
        scalar_type: ScalarType,
    },
}

impl From<ResolvedCallArgument> for UnresolvedCallArgument {
    fn from(value: ResolvedCallArgument) -> Self {
        match value {
            ResolvedCallArgument::Vector {
                memory_region_name, ..
            } => UnresolvedCallArgument::Identifier(memory_region_name),
            ResolvedCallArgument::MemoryReference {
                memory_reference, ..
            } => UnresolvedCallArgument::MemoryReference(memory_reference),
            ResolvedCallArgument::Immediate { value, .. } => {
                UnresolvedCallArgument::Immediate(value)
            }
        }
    }
}

impl Eq for ResolvedCallArgument {}

impl std::hash::Hash for ResolvedCallArgument {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ResolvedCallArgument::Vector {
                memory_region_name,
                vector,
                mutable,
            } => {
                "Vector".hash(state);
                memory_region_name.hash(state);
                vector.hash(state);
                mutable.hash(state);
            }
            ResolvedCallArgument::MemoryReference {
                memory_reference,
                scalar_type,
                mutable,
            } => {
                "MemoryReference".hash(state);
                memory_reference.hash(state);
                scalar_type.hash(state);
                mutable.hash(state);
            }
            ResolvedCallArgument::Immediate { value, scalar_type } => {
                "Immediate".hash(state);
                hash_complex_64(value, state);
                scalar_type.hash(state);
            }
        }
    }
}

fn hash_complex_64<H: std::hash::Hasher>(value: &Complex64, state: &mut H) {
    if value.re.abs() > 0f64 {
        hash_f64(value.re, state);
    }
    if value.im.abs() > 0f64 {
        hash_f64(value.im, state);
    }
}

/// An error that can occur when validating a call instruction.
#[derive(Clone, Debug, PartialEq, thiserror::Error, Eq)]
pub enum CallError {
    /// The specified name is not a valid user identifier.
    #[error(transparent)]
    Name(#[from] IdentifierValidationError),
}

/// A call instruction with a name and arguments.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub struct Call {
    /// The name of the call instruction. This must be a valid user identifier.
    pub name: String,
    /// The arguments of the call instruction.
    pub arguments: Vec<UnresolvedCallArgument>,
}

impl Call {
    /// Create a new call instruction with resolved arguments. This will validate the
    /// name as a user identifier.
    pub fn try_new(
        name: String,
        arguments: Vec<UnresolvedCallArgument>,
    ) -> Result<Self, CallError> {
        validate_user_identifier(name.as_str()).map_err(CallError::Name)?;

        Ok(Self { name, arguments })
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn arguments(&self) -> &[UnresolvedCallArgument] {
        self.arguments.as_slice()
    }
}

/// An error that can occur when resolving a call instruction argument.
#[derive(Clone, Debug, thiserror::Error, PartialEq)]
pub enum CallArgumentError {
    /// The return argument could not be resolved.
    #[error("error resolving return argument: {0:?}")]
    Return(CallArgumentResolutionError),
    /// An argument could not be resolved.
    #[error("error resolving argument {index}: {error:?}")]
    Argument {
        index: usize,
        error: CallArgumentResolutionError,
    },
}

/// An error that can occur when resolving a call instruction to a specific
/// [`ExternSignature`].
#[derive(Debug, thiserror::Error, PartialEq, Clone)]
pub enum CallSignatureError {
    #[error("expected {expected} arguments, found {found}")]
    ParameterCount { expected: usize, found: usize },
    #[error("error resolving arguments: {0:?}")]
    Arguments(Vec<CallArgumentError>),
}

/// An error that can occur when resolving a call instruction, given a complete
/// [`ExternPragmaMap`] for the [`crate::program::Program`].
#[derive(Debug, thiserror::Error, PartialEq, Clone)]
pub enum CallResolutionError {
    /// A matching extern instruction was found, but signature validation failed.
    #[error("call found matching extern instruction for {name}, but signature validation failed: {error:?}")]
    Signature {
        name: String,
        error: CallSignatureError,
    },
    /// No matching extern instruction was found.
    #[error("no extern instruction found with name {0}")]
    NoMatchingExternInstruction(String),
    /// Failed to convernt the [`ExternPragmaMap`] to an [`ExternSignatureMap`].
    #[error(transparent)]
    ExternSignature(#[from] ExternError),
}

#[allow(clippy::manual_try_fold)]
fn convert_unresolved_to_resolved_call_arguments(
    arguments: &[UnresolvedCallArgument],
    signature: &ExternSignature,
    memory_regions: &IndexMap<String, MemoryRegion>,
) -> Result<Vec<ResolvedCallArgument>, CallSignatureError> {
    arguments
        .iter()
        .enumerate()
        .map(|(i, argument)| {
            if i == 0 {
                if let Some(return_type) = signature.return_type {
                    return argument
                        .resolve_return(memory_regions, return_type)
                        .map_err(CallArgumentError::Return);
                }
            }
            let parameter_index = if signature.return_type.is_some() {
                i - 1
            } else {
                i
            };
            let parameter = &signature.parameters[parameter_index];
            argument
                .resolve(memory_regions, parameter)
                .map_err(|error| CallArgumentError::Argument {
                    index: parameter_index,
                    error,
                })
        })
        .fold(
            Ok(Vec::new()),
            |acc: Result<Vec<ResolvedCallArgument>, Vec<CallArgumentError>>,
             result: Result<ResolvedCallArgument, CallArgumentError>| {
                match (acc, result) {
                    (Ok(mut acc), Ok(resolved)) => {
                        acc.push(resolved);
                        Ok(acc)
                    }
                    (Ok(_), Err(error)) => Err(vec![error]),
                    (Err(errors), Ok(_)) => Err(errors),
                    (Err(mut errors), Err(error)) => {
                        errors.push(error);
                        Err(errors)
                    }
                }
            },
        )
        .map_err(CallSignatureError::Arguments)
}

impl Call {
    /// Resolve the [`Call`] instruction to the given [`ExternSignature`].
    fn resolve_to_signature(
        &self,
        signature: &ExternSignature,
        memory_regions: &IndexMap<String, MemoryRegion>,
    ) -> Result<Vec<ResolvedCallArgument>, CallSignatureError> {
        let mut expected_parameter_count = signature.parameters.len();
        if signature.return_type.is_some() {
            expected_parameter_count += 1;
        }

        if self.arguments.len() != expected_parameter_count {
            return Err(CallSignatureError::ParameterCount {
                expected: expected_parameter_count,
                found: self.arguments.len(),
            });
        }

        let resolved_call_arguments = convert_unresolved_to_resolved_call_arguments(
            &self.arguments,
            signature,
            memory_regions,
        )?;

        Ok(resolved_call_arguments)
    }

    /// Resolve the [`Call`] instruction to any of the given [`ExternSignature`]s and memory regions.
    /// If no matching extern instruction is found, return an error.
    pub fn resolve_arguments(
        &self,
        memory_regions: &IndexMap<String, MemoryRegion>,
        extern_signature_map: &ExternSignatureMap,
    ) -> Result<Vec<ResolvedCallArgument>, CallResolutionError> {
        let extern_signature = extern_signature_map
            .0
            .get(self.name.as_str())
            .ok_or_else(|| CallResolutionError::NoMatchingExternInstruction(self.name.clone()))?;

        self.resolve_to_signature(extern_signature, memory_regions)
            .map_err(|error| CallResolutionError::Signature {
                name: self.name.clone(),
                error,
            })
    }

    /// Return the [`MemoryAccesses`] for the [`Call`] instruction given the [`ExternSignatureMap`].
    /// This assumes ALL parameters are read, including mutable parameters.
    pub(crate) fn get_memory_accesses(
        &self,
        extern_signatures: &ExternSignatureMap,
    ) -> Result<MemoryAccesses, CallResolutionError> {
        let extern_signature = extern_signatures
            .0
            .get(self.name.as_str())
            .ok_or_else(|| CallResolutionError::NoMatchingExternInstruction(self.name.clone()))?;

        let mut reads = HashSet::new();
        let mut writes = HashSet::new();
        let mut arguments = self.arguments.iter();
        if extern_signature.return_type.is_some() {
            if let Some(argument) = self.arguments.first() {
                arguments.next();
                match argument {
                    UnresolvedCallArgument::MemoryReference(memory_reference) => {
                        reads.insert(memory_reference.name.clone());
                        writes.insert(memory_reference.name.clone());
                    }
                    UnresolvedCallArgument::Identifier(identifier) => {
                        reads.insert(identifier.clone());
                        writes.insert(identifier.clone());
                    }
                    _ => {}
                }
            }
        }
        for (argument, parameter) in std::iter::zip(arguments, extern_signature.parameters.iter()) {
            match argument {
                UnresolvedCallArgument::MemoryReference(memory_reference) => {
                    reads.insert(memory_reference.name.clone());
                    if parameter.mutable {
                        writes.insert(memory_reference.name.clone());
                    }
                }
                UnresolvedCallArgument::Identifier(identifier) => {
                    reads.insert(identifier.clone());
                    if parameter.mutable {
                        writes.insert(identifier.clone());
                    }
                }
                _ => {}
            }
        }
        Ok(MemoryAccesses {
            reads,
            writes,
            captures: HashSet::new(),
        })
    }
}

impl Quil for Call {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "CALL {}", self.name)?;
        for argument in self.arguments.as_slice() {
            write!(f, " ")?;
            argument.write(f, fall_back_to_debug)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::PragmaArgument;
    use rstest::*;

    /// Test cases for the [`ExternSignature`] Quil representation.
    struct ExternSignatureQuilTestCase {
        /// The extern signature to test.
        signature: ExternSignature,
        /// The expected Quil representation.
        expected: &'static str,
    }

    impl ExternSignatureQuilTestCase {
        /// Signature with return and parameters
        fn case_01() -> Self {
            Self {
                signature: ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![
                        ExternParameter {
                            name: "bar".to_string(),
                            mutable: false,
                            data_type: ExternParameterType::Scalar(ScalarType::Integer),
                        },
                        ExternParameter {
                            name: "baz".to_string(),
                            mutable: true,
                            data_type: ExternParameterType::FixedLengthVector(Vector {
                                data_type: ScalarType::Bit,
                                length: 2,
                            }),
                        },
                    ],
                },
                expected: "INTEGER (bar : INTEGER, baz : mut BIT[2])",
            }
        }

        /// Signature with only parameters
        fn case_02() -> Self {
            let signature = ExternSignature {
                return_type: None,
                parameters: vec![
                    ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Integer),
                    },
                    ExternParameter {
                        name: "baz".to_string(),
                        mutable: true,
                        data_type: ExternParameterType::FixedLengthVector(Vector {
                            data_type: ScalarType::Bit,
                            length: 2,
                        }),
                    },
                ],
            };
            Self {
                signature,
                expected: "(bar : INTEGER, baz : mut BIT[2])",
            }
        }

        /// Signature with return only
        fn case_03() -> Self {
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![],
            };
            Self {
                signature,
                expected: "INTEGER",
            }
        }

        /// Signature with no return nor parameters
        fn case_04() -> Self {
            let signature = ExternSignature {
                return_type: None,
                parameters: vec![],
            };
            Self {
                signature,
                expected: "",
            }
        }

        /// Variable length vector
        fn case_05() -> Self {
            let signature = ExternSignature {
                return_type: None,
                parameters: vec![ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::VariableLengthVector(ScalarType::Integer),
                }],
            };
            Self {
                signature,
                expected: "(bar : INTEGER[])",
            }
        }
    }

    /// Test that the Quil representation of an [`ExternSignature`] is as expected.
    #[rstest]
    #[case(ExternSignatureQuilTestCase::case_01())]
    #[case(ExternSignatureQuilTestCase::case_02())]
    #[case(ExternSignatureQuilTestCase::case_03())]
    #[case(ExternSignatureQuilTestCase::case_04())]
    #[case(ExternSignatureQuilTestCase::case_05())]
    #[case(ExternSignatureQuilTestCase::case_05())]
    fn test_extern_signature_quil(#[case] test_case: ExternSignatureQuilTestCase) {
        assert_eq!(
            test_case
                .signature
                .to_quil()
                .expect("must be able to call to quil"),
            test_case.expected.to_string()
        );
    }

    /// Test cases for the [`Call`] Quil representation.
    struct CallQuilTestCase {
        /// The call instruction to test.
        call: Call,
        /// The expected Quil representation.
        expected: &'static str,
    }

    impl CallQuilTestCase {
        fn case_01() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "bar".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("baz".to_string()),
                ],
            };
            Self {
                call,
                expected: "CALL foo bar[0] 2 baz",
            }
        }

        fn case_02() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "bar".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Identifier("baz".to_string()),
                ],
            };
            Self {
                call,
                expected: "CALL foo bar[0] baz",
            }
        }

        fn case_03() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "bar".to_string(),
                    index: 0,
                })],
            };
            Self {
                call,
                expected: "CALL foo bar[0]",
            }
        }

        /// No arguments.
        fn case_04() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![],
            };

            Self {
                call,
                expected: "CALL foo",
            }
        }
    }

    /// Test that the Quil representation of a [`Call`] instruction is as expected.
    #[rstest]
    #[case(CallQuilTestCase::case_01())]
    #[case(CallQuilTestCase::case_02())]
    #[case(CallQuilTestCase::case_03())]
    #[case(CallQuilTestCase::case_04())]
    fn test_call_quil(#[case] test_case: CallQuilTestCase) {
        assert_eq!(
            test_case
                .call
                .to_quil()
                .expect("must be able to call to quil"),
            test_case.expected.to_string()
        );
    }

    /// Build a set of memory regions for testing.
    fn build_declarations() -> IndexMap<String, MemoryRegion> {
        [
            ("integer", Vector::new(ScalarType::Integer, 3)),
            ("real", Vector::new(ScalarType::Real, 3)),
            ("bit", Vector::new(ScalarType::Bit, 3)),
            ("octet", Vector::new(ScalarType::Octet, 3)),
        ]
        .into_iter()
        .map(|(name, vector)| (name.to_string(), MemoryRegion::new(vector, None)))
        .collect()
    }

    /// Test cases for resolving call arguments.
    struct ArgumentResolutionTestCase {
        call_argument: UnresolvedCallArgument,
        extern_parameter: ExternParameter,
        expected: Result<ResolvedCallArgument, CallArgumentResolutionError>,
    }

    impl ArgumentResolutionTestCase {
        /// Memory reference as scalar
        fn case_01() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                }),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Ok(ResolvedCallArgument::MemoryReference {
                    memory_reference: MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    },
                    scalar_type: ScalarType::Integer,
                    mutable: false,
                }),
            }
        }

        /// Identifier as vector
        fn case_02() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::Identifier("real".to_string()),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Real,
                        length: 3,
                    }),
                },
                expected: Ok(ResolvedCallArgument::Vector {
                    memory_region_name: "real".to_string(),
                    vector: Vector {
                        data_type: ScalarType::Real,
                        length: 3,
                    },
                    mutable: false,
                }),
            }
        }

        /// Immediate value as scalar
        fn case_03() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Ok(ResolvedCallArgument::Immediate {
                    value: Complex64::new(2.0, 0.0),
                    scalar_type: ScalarType::Integer,
                }),
            }
        }

        /// Undeclared identifier
        fn case_04() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::Identifier("undeclared".to_string()),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Real,
                        length: 3,
                    }),
                },
                expected: Err(CallArgumentResolutionError::UndeclaredMemoryReference(
                    "undeclared".to_string(),
                )),
            }
        }

        /// Undeclared memory reference
        fn case_05() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "undeclared".to_string(),
                    index: 0,
                }),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Err(CallArgumentResolutionError::UndeclaredMemoryReference(
                    "undeclared".to_string(),
                )),
            }
        }

        /// Vector data type mismatch
        fn case_06() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::Identifier("integer".to_string()),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Real,
                        length: 3,
                    }),
                },
                expected: Err(CallArgumentResolutionError::MismatchedVector {
                    expected: Vector {
                        data_type: ScalarType::Real,
                        length: 3,
                    },
                    found: Vector {
                        data_type: ScalarType::Integer,
                        length: 3,
                    },
                }),
            }
        }

        /// Vector length mismatch
        fn case_07() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::Identifier("integer".to_string()),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Integer,
                        length: 4,
                    }),
                },
                expected: Err(CallArgumentResolutionError::MismatchedVector {
                    expected: Vector {
                        data_type: ScalarType::Integer,
                        length: 4,
                    },
                    found: Vector {
                        data_type: ScalarType::Integer,
                        length: 3,
                    },
                }),
            }
        }

        /// Scalar data type mismatch
        fn case_08() -> Self {
            ArgumentResolutionTestCase {
                call_argument: UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "octet".to_string(),
                    index: 0,
                }),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Err(CallArgumentResolutionError::MismatchedScalar {
                    expected: ScalarType::Integer,
                    found: ScalarType::Octet,
                }),
            }
        }

        /// Scalar arguments may be passed as identifiers, in which case `0` index is
        /// inferred.
        fn case_09() -> Self {
            let call_argument = UnresolvedCallArgument::Identifier("integer".to_string());
            ArgumentResolutionTestCase {
                call_argument: call_argument.clone(),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Ok(ResolvedCallArgument::MemoryReference {
                    memory_reference: MemoryReference::new("integer".to_string(), 0),
                    scalar_type: ScalarType::Integer,
                    mutable: false,
                }),
            }
        }

        /// Vector arguments must be passed as identifiers, not memory references.
        fn case_10() -> Self {
            let call_argument = UnresolvedCallArgument::MemoryReference(MemoryReference {
                name: "integer".to_string(),
                index: 0,
            });
            ArgumentResolutionTestCase {
                call_argument: call_argument.clone(),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Integer,
                        length: 3,
                    }),
                },
                expected: Err(CallArgumentResolutionError::InvalidVectorArgument(
                    call_argument,
                )),
            }
        }

        /// Vector arguments must be passed as identifiers, not immediate values.
        fn case_11() -> Self {
            let call_argument = UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0));
            ArgumentResolutionTestCase {
                call_argument: call_argument.clone(),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::FixedLengthVector(Vector {
                        data_type: ScalarType::Integer,
                        length: 3,
                    }),
                },
                expected: Err(CallArgumentResolutionError::InvalidVectorArgument(
                    call_argument,
                )),
            }
        }

        /// Variable vector arguments are resolved to a specific vector length based on the
        /// declaration (see [`build_declarations`]).
        fn case_12() -> Self {
            let call_argument = UnresolvedCallArgument::Identifier("integer".to_string());
            ArgumentResolutionTestCase {
                call_argument: call_argument.clone(),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::VariableLengthVector(ScalarType::Integer),
                },
                expected: Ok(ResolvedCallArgument::Vector {
                    memory_region_name: "integer".to_string(),
                    mutable: false,
                    vector: Vector {
                        data_type: ScalarType::Integer,
                        length: 3,
                    },
                }),
            }
        }

        /// Immediate arguments cannot be passed for mutable parameters.
        fn case_13() -> Self {
            let call_argument = UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0));
            ArgumentResolutionTestCase {
                call_argument: call_argument.clone(),
                extern_parameter: ExternParameter {
                    name: "bar".to_string(),
                    mutable: true,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                },
                expected: Err(CallArgumentResolutionError::ImmediateArgumentForMutable(
                    "bar".to_string(),
                )),
            }
        }
    }

    /// Test resolution of call arguments.
    #[rstest]
    #[case(ArgumentResolutionTestCase::case_01())]
    #[case(ArgumentResolutionTestCase::case_02())]
    #[case(ArgumentResolutionTestCase::case_03())]
    #[case(ArgumentResolutionTestCase::case_04())]
    #[case(ArgumentResolutionTestCase::case_05())]
    #[case(ArgumentResolutionTestCase::case_06())]
    #[case(ArgumentResolutionTestCase::case_07())]
    #[case(ArgumentResolutionTestCase::case_08())]
    #[case(ArgumentResolutionTestCase::case_09())]
    #[case(ArgumentResolutionTestCase::case_10())]
    #[case(ArgumentResolutionTestCase::case_11())]
    #[case(ArgumentResolutionTestCase::case_12())]
    #[case(ArgumentResolutionTestCase::case_13())]
    fn test_argument_resolution(#[case] test_case: ArgumentResolutionTestCase) {
        let memory_regions = build_declarations();
        let found = test_case
            .call_argument
            .resolve(&memory_regions, &test_case.extern_parameter);
        match (test_case.expected, found) {
            (Ok(expected), Ok(found)) => assert_eq!(expected, found),
            (Ok(expected), Err(found)) => {
                panic!("expected resolution {:?}, found err {:?}", expected, found)
            }
            (Err(expected), Ok(found)) => {
                panic!("expected err {:?}, found resolution {:?}", expected, found)
            }
            (Err(expected), Err(found)) => assert_eq!(expected, found),
        }
    }

    /// Test cases for resolving return arguments.
    struct ReturnArgumentResolutionTestCase {
        /// The call argument to resolve.
        call_argument: UnresolvedCallArgument,
        /// The return type of the function.
        return_type: ScalarType,
        /// The expected result of the resolution.
        expected: Result<ResolvedCallArgument, CallArgumentResolutionError>,
    }

    impl ReturnArgumentResolutionTestCase {
        /// Memory reference is ok.
        fn case_01() -> Self {
            let call_argument = UnresolvedCallArgument::MemoryReference(MemoryReference {
                name: "integer".to_string(),
                index: 0,
            });
            let expected = Ok(ResolvedCallArgument::MemoryReference {
                memory_reference: MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                },
                scalar_type: ScalarType::Integer,
                mutable: true,
            });
            Self {
                call_argument,
                return_type: ScalarType::Integer,
                expected,
            }
        }

        /// Immediate value is not ok.
        fn case_02() -> Self {
            let call_argument = UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0));
            let expected = Err(CallArgumentResolutionError::ReturnArgument {
                found: call_argument.clone(),
            });
            Self {
                call_argument,
                return_type: ScalarType::Integer,
                expected,
            }
        }

        /// Allow plain identifiers to be upcast to memory references.
        fn case_03() -> Self {
            let call_argument = UnresolvedCallArgument::Identifier("integer".to_string());
            let expected = Ok(ResolvedCallArgument::MemoryReference {
                memory_reference: MemoryReference::new("integer".to_string(), 0),
                scalar_type: ScalarType::Integer,
                mutable: true,
            });
            Self {
                call_argument,
                return_type: ScalarType::Integer,
                expected,
            }
        }
    }

    /// Test resolution of return arguments.
    #[rstest]
    #[case(ReturnArgumentResolutionTestCase::case_01())]
    #[case(ReturnArgumentResolutionTestCase::case_02())]
    #[case(ReturnArgumentResolutionTestCase::case_03())]
    fn test_return_argument_resolution(#[case] test_case: ReturnArgumentResolutionTestCase) {
        let memory_regions = build_declarations();

        let found = test_case
            .call_argument
            .resolve_return(&memory_regions, test_case.return_type);
        match (test_case.expected, found) {
            (Ok(expected), Ok(found)) => assert_eq!(expected, found),
            (Ok(expected), Err(found)) => {
                panic!("expected resolution {:?}, found err {:?}", expected, found)
            }
            (Err(expected), Ok(found)) => {
                panic!("expected err {:?}, found resolution {:?}", expected, found)
            }
            (Err(expected), Err(found)) => assert_eq!(expected, found),
        }
    }

    /// Test cases for resolving call arguments to a specific signature.
    struct ResolveToSignatureTestCase {
        /// The call instruction to resolve.
        call: Call,
        /// The signature to resolve to.
        signature: ExternSignature,
        /// The expected result of the resolution.
        expected: Result<Vec<ResolvedCallArgument>, CallSignatureError>,
    }

    impl ResolveToSignatureTestCase {
        /// Valid match with return and parameters
        fn case_01() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![
                    ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Integer),
                    },
                    ExternParameter {
                        name: "baz".to_string(),
                        mutable: true,
                        data_type: ExternParameterType::FixedLengthVector(Vector {
                            data_type: ScalarType::Bit,
                            length: 3,
                        }),
                    },
                ],
            };
            let resolved = vec![
                ResolvedCallArgument::MemoryReference {
                    memory_reference: MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    },
                    scalar_type: ScalarType::Integer,
                    mutable: true,
                },
                ResolvedCallArgument::Immediate {
                    value: Complex64::new(2.0, 0.0),
                    scalar_type: ScalarType::Integer,
                },
                ResolvedCallArgument::Vector {
                    memory_region_name: "bit".to_string(),
                    vector: Vector {
                        data_type: ScalarType::Bit,
                        length: 3,
                    },
                    mutable: true,
                },
            ];
            Self {
                call,
                signature,
                expected: Ok(resolved),
            }
        }

        /// Valid match with parameteters only
        fn case_02() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ],
            };
            let signature = ExternSignature {
                return_type: None,
                parameters: vec![
                    ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Integer),
                    },
                    ExternParameter {
                        name: "baz".to_string(),
                        mutable: true,
                        data_type: ExternParameterType::FixedLengthVector(Vector {
                            data_type: ScalarType::Bit,
                            length: 3,
                        }),
                    },
                ],
            };
            let resolved = vec![
                ResolvedCallArgument::Immediate {
                    value: Complex64::new(2.0, 0.0),
                    scalar_type: ScalarType::Integer,
                },
                ResolvedCallArgument::Vector {
                    memory_region_name: "bit".to_string(),
                    vector: Vector {
                        data_type: ScalarType::Bit,
                        length: 3,
                    },
                    mutable: true,
                },
            ];
            Self {
                call,
                signature,
                expected: Ok(resolved),
            }
        }

        /// Valid match with return only
        fn case_03() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                })],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![],
            };
            let resolved = vec![ResolvedCallArgument::MemoryReference {
                memory_reference: MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                },
                scalar_type: ScalarType::Integer,
                mutable: true,
            }];
            Self {
                call,
                signature,
                expected: Ok(resolved),
            }
        }

        /// Parameter count mismatch with return and parameters
        fn case_04() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                }],
            };

            Self {
                call,
                signature,
                expected: Err(CallSignatureError::ParameterCount {
                    expected: 2,
                    found: 3,
                }),
            }
        }

        /// Parameter count mismatch return only
        fn case_05() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                ],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![],
            };

            Self {
                call,
                signature,
                expected: Err(CallSignatureError::ParameterCount {
                    expected: 1,
                    found: 2,
                }),
            }
        }

        /// Parameter count mismatch parameters only
        fn case_06() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ],
            };
            let signature = ExternSignature {
                return_type: None,
                parameters: vec![ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                }],
            };

            Self {
                call,
                signature,
                expected: Err(CallSignatureError::ParameterCount {
                    expected: 1,
                    found: 3,
                }),
            }
        }

        /// Argument mismatch
        fn case_07() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Real),
                }],
            };

            Self {
                call,
                signature,
                expected: Err(CallSignatureError::Arguments(vec![
                    CallArgumentError::Return(CallArgumentResolutionError::ReturnArgument {
                        found: UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    }),
                    CallArgumentError::Argument {
                        index: 0,
                        error: CallArgumentResolutionError::MismatchedScalar {
                            expected: ScalarType::Real,
                            found: ScalarType::Bit,
                        },
                    },
                ])),
            }
        }
    }

    /// Test resolution of `Call` instructions to a specific signature.
    #[rstest]
    #[case(ResolveToSignatureTestCase::case_01())]
    #[case(ResolveToSignatureTestCase::case_02())]
    #[case(ResolveToSignatureTestCase::case_03())]
    #[case(ResolveToSignatureTestCase::case_04())]
    #[case(ResolveToSignatureTestCase::case_05())]
    #[case(ResolveToSignatureTestCase::case_06())]
    #[case(ResolveToSignatureTestCase::case_07())]
    fn test_assert_matching_signature(#[case] test_case: ResolveToSignatureTestCase) {
        let memory_regions = build_declarations();
        let found = test_case
            .call
            .resolve_to_signature(&test_case.signature, &memory_regions);
        match (test_case.expected, found) {
            (Ok(_), Ok(_)) => {}
            (Ok(expected), Err(found)) => {
                panic!("expected resolution {:?}, found err {:?}", expected, found)
            }
            (Err(expected), Ok(found)) => {
                panic!("expected err {:?}, found resolution {:?}", expected, found)
            }
            (Err(expected), Err(found)) => assert_eq!(expected, found),
        }
    }

    /// Test cases for call resolution against an [`ExternSignatureMap`].
    struct CallResolutionTestCase {
        /// The call instruction to resolve.
        call: Call,
        /// The set of extern definitions to resolve against.
        extern_signature_map: ExternSignatureMap,
        /// The expected result of the resolution.
        expected: Result<Vec<ResolvedCallArgument>, CallResolutionError>,
    }

    impl CallResolutionTestCase {
        /// Valid resolution
        fn case_01() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                })],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![],
            };
            let resolved = vec![ResolvedCallArgument::MemoryReference {
                memory_reference: MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                },
                scalar_type: ScalarType::Integer,
                mutable: true,
            }];
            Self {
                call,
                extern_signature_map: ExternSignatureMap(
                    [("foo".to_string(), signature)].iter().cloned().collect(),
                ),
                expected: Ok(resolved),
            }
        }

        /// Signature does not match
        fn case_02() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: vec![UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                })],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Real),
                parameters: vec![],
            };
            Self {
                call,
                extern_signature_map: ExternSignatureMap(
                    [("foo".to_string(), signature)].iter().cloned().collect(),
                ),
                expected: Err(CallResolutionError::Signature {
                    name: "foo".to_string(),
                    error: CallSignatureError::Arguments(vec![CallArgumentError::Return(
                        CallArgumentResolutionError::MismatchedScalar {
                            expected: ScalarType::Real,
                            found: ScalarType::Integer,
                        },
                    )]),
                }),
            }
        }

        /// No corresponding extern definition
        fn case_03() -> Self {
            let call = Call {
                name: "undeclared".to_string(),
                arguments: vec![UnresolvedCallArgument::MemoryReference(MemoryReference {
                    name: "integer".to_string(),
                    index: 0,
                })],
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Real),
                parameters: vec![],
            };
            Self {
                call,
                extern_signature_map: ExternSignatureMap(
                    [("foo".to_string(), signature)].iter().cloned().collect(),
                ),
                expected: Err(CallResolutionError::NoMatchingExternInstruction(
                    "undeclared".to_string(),
                )),
            }
        }
    }

    /// Test resolution of [`Call`] instructions against a set of extern definitions.
    #[rstest]
    #[case(CallResolutionTestCase::case_01())]
    #[case(CallResolutionTestCase::case_02())]
    #[case(CallResolutionTestCase::case_03())]
    fn test_call_resolution(#[case] test_case: CallResolutionTestCase) {
        let memory_regions = build_declarations();
        let found = test_case
            .call
            .resolve_arguments(&memory_regions, &test_case.extern_signature_map);
        match (test_case.expected, found) {
            (Ok(expected), Ok(found)) => {
                assert_eq!(expected, found);
            }
            (Ok(expected), Err(found)) => {
                panic!("expected resolution {:?}, found err {:?}", expected, found)
            }
            (Err(expected), Ok(_)) => {
                panic!(
                    "expected err {:?}, found resolution {:?}",
                    expected, test_case.call.arguments
                )
            }
            (Err(expected), Err(found)) => assert_eq!(expected, found),
        }
    }

    /// Test cases for converting [`ExternPragmaMap`] to [`ExternSignatureMap`].
    struct ExternPragmaMapConverstionTestCase {
        /// The set of extern definitions to validate.
        extern_pragma_map: ExternPragmaMap,
        /// The expected result of the validation.
        expected: Result<ExternSignatureMap, ExternError>,
    }

    impl ExternPragmaMapConverstionTestCase {
        /// Valid [`ExternPragmaMap`]s.
        fn case_01() -> Self {
            let pragma1 = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: Some("(bar : INTEGER)".to_string()),
            };
            let signature1 = ExternSignature {
                return_type: None,
                parameters: vec![ExternParameter {
                    name: "bar".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Integer),
                }],
            };
            let pragma2 = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("baz".to_string())],
                data: Some("REAL (biz : REAL)".to_string()),
            };
            let signature2 = ExternSignature {
                return_type: Some(ScalarType::Real),
                parameters: vec![ExternParameter {
                    name: "biz".to_string(),
                    mutable: false,
                    data_type: ExternParameterType::Scalar(ScalarType::Real),
                }],
            };
            let pragma3 = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("buzz".to_string())],
                data: Some("OCTET".to_string()),
            };
            let signature3 = ExternSignature {
                return_type: Some(ScalarType::Octet),
                parameters: vec![],
            };
            Self {
                extern_pragma_map: ExternPragmaMap(
                    [("foo", pragma1), ("baz", pragma2), ("buzz", pragma3)]
                        .into_iter()
                        .map(|(name, pragma)| (Some(name.to_string()), pragma))
                        .collect(),
                ),
                expected: Ok(ExternSignatureMap(
                    [
                        ("foo", signature1),
                        ("baz", signature2),
                        ("buzz", signature3),
                    ]
                    .into_iter()
                    .map(|(name, signature)| (name.to_string(), signature))
                    .collect(),
                )),
            }
        }

        /// No Signature
        fn case_02() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: None,
            };
            let expected = Err(ExternError::NoSignature);
            Self {
                extern_pragma_map: ExternPragmaMap(
                    [(Some("foo".to_string()), pragma)].into_iter().collect(),
                ),
                expected,
            }
        }

        /// No return nor parameters
        fn case_03() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: Some("()".to_string()),
            };
            let expected = Err(ExternError::NoReturnOrParameters);
            Self {
                extern_pragma_map: ExternPragmaMap(
                    [(Some("foo".to_string()), pragma)].into_iter().collect(),
                ),
                expected,
            }
        }

        /// No name
        fn case_04() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![],
                data: Some("(bar : REAL)".to_string()),
            };
            let expected = Err(ExternError::NoName);
            Self {
                extern_pragma_map: ExternPragmaMap([(None, pragma)].into_iter().collect()),
                expected,
            }
        }

        /// Not extern
        fn case_05() -> Self {
            let pragma = Pragma {
                name: "NOTEXTERN".to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: Some("(bar : REAL)".to_string()),
            };
            let expected = Err(ExternError::NoName);
            Self {
                extern_pragma_map: ExternPragmaMap([(None, pragma)].into_iter().collect()),
                expected,
            }
        }

        /// Extraneous arguments
        fn case_06() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![
                    PragmaArgument::Identifier("foo".to_string()),
                    PragmaArgument::Identifier("bar".to_string()),
                ],
                data: Some("OCTET".to_string()),
            };
            let expected = Err(ExternError::NoName);
            Self {
                extern_pragma_map: ExternPragmaMap([(None, pragma)].into_iter().collect()),
                expected,
            }
        }

        /// Integer is not a name
        fn case_07() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Integer(0)],
                data: Some("OCTET".to_string()),
            };
            let expected = Err(ExternError::NoName);
            Self {
                extern_pragma_map: ExternPragmaMap([(None, pragma)].into_iter().collect()),
                expected,
            }
        }

        /// Lex error
        fn case_08() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: Some("OCTET (ㆆ _ ㆆ)".to_string()),
            };
            let expected = Err(ExternSignature::from_str("OCTET (ㆆ _ ㆆ)").unwrap_err());
            Self {
                extern_pragma_map: ExternPragmaMap(
                    [(Some("foo".to_string()), pragma)].into_iter().collect(),
                ),
                expected,
            }
        }

        /// Syntax error - missing parenthesis
        fn case_09() -> Self {
            let pragma = Pragma {
                name: RESERVED_PRAGMA_EXTERN.to_string(),
                arguments: vec![PragmaArgument::Identifier("foo".to_string())],
                data: Some("OCTET (bar : INTEGER".to_string()),
            };
            let expected = Err(ExternSignature::from_str("OCTET (bar : INTEGER").unwrap_err());
            Self {
                extern_pragma_map: ExternPragmaMap(
                    [(Some("foo".to_string()), pragma)].into_iter().collect(),
                ),
                expected,
            }
        }
    }

    /// Test conversion of [`ExternPragmaMap`] to [`ExternSignatureMap`].
    #[rstest]
    #[case(ExternPragmaMapConverstionTestCase::case_01())]
    #[case(ExternPragmaMapConverstionTestCase::case_02())]
    #[case(ExternPragmaMapConverstionTestCase::case_03())]
    #[case(ExternPragmaMapConverstionTestCase::case_04())]
    #[case(ExternPragmaMapConverstionTestCase::case_05())]
    #[case(ExternPragmaMapConverstionTestCase::case_06())]
    #[case(ExternPragmaMapConverstionTestCase::case_07())]
    #[case(ExternPragmaMapConverstionTestCase::case_08())]
    #[case(ExternPragmaMapConverstionTestCase::case_09())]
    fn test_extern_signature_map_validation(#[case] test_case: ExternPragmaMapConverstionTestCase) {
        let found = ExternSignatureMap::try_from(test_case.extern_pragma_map);
        match (test_case.expected, found) {
            (Ok(expected), Ok(found)) => {
                assert_eq!(expected, found);
            }
            (Ok(_), Err(found)) => {
                panic!("expected valid, found err {:?}", found)
            }
            (Err(expected), Ok(_)) => {
                panic!("expected err {:?}, found valid", expected)
            }
            (Err(expected), Err((_, found))) => assert_eq!(expected, found),
        }
    }

    /// Test cases for parsing [`ExternSignature`]s.
    struct ExternSignatureFromStrTestCase {
        /// This string to parse.
        input: &'static str,
        /// The parsing result.
        expected: Result<ExternSignature, ExternError>,
    }

    impl ExternSignatureFromStrTestCase {
        /// Empty signature
        fn case_01() -> Self {
            Self {
                input: "",
                expected: Err(ExternError::NoReturnOrParameters),
            }
        }

        /// Empty signature with parentheses
        fn case_02() -> Self {
            Self {
                input: "()",
                expected: Err(ExternError::NoReturnOrParameters),
            }
        }

        /// Return without parameters
        fn case_03() -> Self {
            Self {
                input: "INTEGER",
                expected: Ok(crate::instruction::ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![],
                }),
            }
        }

        /// Return with empty parentheses
        fn case_04() -> Self {
            Self {
                input: "INTEGER ()",
                expected: Ok(crate::instruction::ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![],
                }),
            }
        }

        /// Return with parameters
        fn case_05() -> Self {
            Self {
                input: "INTEGER (bar: REAL, baz: BIT[10], biz: mut OCTET)",
                expected: Ok(crate::instruction::ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![
                        ExternParameter {
                            name: "bar".to_string(),
                            mutable: false,
                            data_type: ExternParameterType::Scalar(ScalarType::Real),
                        },
                        ExternParameter {
                            name: "baz".to_string(),
                            mutable: false,
                            data_type: ExternParameterType::FixedLengthVector(Vector {
                                data_type: ScalarType::Bit,
                                length: 10,
                            }),
                        },
                        ExternParameter {
                            name: "biz".to_string(),
                            mutable: true,
                            data_type: ExternParameterType::Scalar(ScalarType::Octet),
                        },
                    ],
                }),
            }
        }

        /// Parameters without return
        fn case_06() -> Self {
            Self {
                input: "(bar: REAL, baz: BIT[10], biz : mut OCTET)",
                expected: Ok(crate::instruction::ExternSignature {
                    return_type: None,
                    parameters: vec![
                        ExternParameter {
                            name: "bar".to_string(),
                            mutable: false,
                            data_type: ExternParameterType::Scalar(ScalarType::Real),
                        },
                        ExternParameter {
                            name: "baz".to_string(),
                            mutable: false,
                            data_type: ExternParameterType::FixedLengthVector(Vector {
                                data_type: ScalarType::Bit,
                                length: 10,
                            }),
                        },
                        ExternParameter {
                            name: "biz".to_string(),
                            mutable: true,
                            data_type: ExternParameterType::Scalar(ScalarType::Octet),
                        },
                    ],
                }),
            }
        }

        /// Variable length vector.
        fn case_07() -> Self {
            Self {
                input: "(bar : mut REAL[])",
                expected: Ok(crate::instruction::ExternSignature {
                    return_type: None,
                    parameters: vec![ExternParameter {
                        name: "bar".to_string(),
                        mutable: true,
                        data_type: ExternParameterType::VariableLengthVector(ScalarType::Real),
                    }],
                }),
            }
        }
    }

    /// Test parsing of `PRAGMA EXTERN` instructions.
    #[rstest]
    #[case(ExternSignatureFromStrTestCase::case_01())]
    #[case(ExternSignatureFromStrTestCase::case_02())]
    #[case(ExternSignatureFromStrTestCase::case_03())]
    #[case(ExternSignatureFromStrTestCase::case_04())]
    #[case(ExternSignatureFromStrTestCase::case_05())]
    #[case(ExternSignatureFromStrTestCase::case_06())]
    #[case(ExternSignatureFromStrTestCase::case_07())]
    fn test_parse_reserved_pragma_extern(#[case] test_case: ExternSignatureFromStrTestCase) {
        match (
            test_case.expected,
            ExternSignature::from_str(test_case.input),
        ) {
            (Ok(expected), Ok(parsed)) => {
                assert_eq!(expected, parsed);
            }
            (Ok(expected), Err(e)) => {
                panic!("Expected {:?}, got error: {:?}", expected, e);
            }
            (Err(expected), Ok(parsed)) => {
                panic!("Expected error: {:?}, got {:?}", expected, parsed);
            }
            (Err(expected), Err(found)) => {
                let expected = format!("{expected:?}");
                let found = format!("{found:?}");
                assert!(
                    found.contains(&expected),
                    "`{}` not in `{}`",
                    expected,
                    found
                );
            }
        }
    }
}
