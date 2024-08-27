/// This module provides support for the `CALL` instruction and the reserved `PRAGMA EXTERN` instruction.
///
/// For additional detail on its design and specification see:
///
/// * [Quil specification "Other"](https://github.com/quil-lang/quil/blob/7f532c7cdde9f51eae6abe7408cc868fba9f91f6/specgen/spec/sec-other.s_)
/// * [Quil EXTERN / CALL RFC](https://github.com/quil-lang/quil/blob/master/rfcs/extern-call.md)
/// * https://github.com/quil-lang/quil/pull/69
use std::{collections::HashSet, str::FromStr};

use indexmap::IndexMap;
use nom_locate::LocatedSpan;
use num_complex::Complex64;

use crate::{
    expression::format_complex,
    hash::hash_f64,
    parser::{lex, InternalParseError, ParserErrorKind, ParserInput},
    program::{disallow_leftover, MemoryRegion, SyntaxError},
    quil::Quil,
    validation::identifier::{validate_user_identifier, IdentifierValidationError},
};

use super::{MemoryReference, ScalarType, Vector};

/// A parameter type within an extern signature.
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum ExternParameterType {
    Scalar(ScalarType),
    FixedLengthVector(Vector),
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
                write!(f, "[]").map_err(Into::into)
            }
        }
    }
}

/// An extern parameter with a name, mutability, and data type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExternParameter {
    /// The name of the parameter. This must be a valid user identifier.
    pub name: String,
    /// Whether the parameter is mutable.
    pub mutable: bool,
    /// The data type of the parameter.
    pub data_type: ExternParameterType,
}

impl ExternParameter {
    /// Create a new extern parameter.
    pub fn new(name: String, mutable: bool, data_type: ExternParameterType) -> Self {
        Self {
            name,
            mutable,
            data_type,
        }
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
    pub return_type: Option<ScalarType>,
    /// The parameters of the extern signature.
    pub parameters: Vec<ExternParameter>,
}

impl ExternSignature {
    /// Create a new extern signature.
    pub fn new(return_type: Option<ScalarType>, parameters: Vec<ExternParameter>) -> Self {
        Self {
            return_type,
            parameters,
        }
    }

    fn has_return_or_parameters(&self) -> bool {
        self.return_type.is_some() || !self.parameters.is_empty()
    }
}

/// An error that can occur when parsing an extern signature.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ExternSignatureError {
    /// An error occurred while parsing the contents of the extern signature.
    #[error("invalid extern signature syntax: {0}")]
    Syntax(SyntaxError<ExternSignature>),
    /// An error occurred while lexing the extern signature.
    #[error("failed to lex extern signature: {0}")]
    Lex(crate::parser::LexError),
}

impl ExternSignatureError {
    pub(crate) fn into_internal_parse_error(
        self,
        input: ParserInput<'_>,
    ) -> InternalParseError<'_> {
        InternalParseError::from_kind(input, ParserErrorKind::from(Box::new(self)))
    }
}

impl FromStr for ExternSignature {
    type Err = ExternSignatureError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let signature_input = LocatedSpan::new(s);
        let signature_tokens = lex(signature_input).map_err(ExternSignatureError::Lex)?;
        let signature = disallow_leftover(
            crate::parser::reserved_pragma_extern::parse_extern_signature(
                signature_tokens.as_slice(),
            )
            .map_err(crate::parser::ParseError::from_nom_internal_err),
        )
        .map_err(ExternSignatureError::Syntax)?;
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

/// An extern definition with a name and optional signature. Note, this is not a
/// Quil instruction or command, though it may become so in the future. Currently,
/// it is defined as a reserved pragma.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ExternDefinition {
    /// The name of the extern definition. This must be a valid user identifier.
    pub name: String,
    /// The signature of the extern definition, if any.
    pub signature: Option<ExternSignature>,
}

impl ExternDefinition {
    pub fn try_new(
        name: String,
        signature: Option<ExternSignature>,
    ) -> Result<Self, ExternValidationError> {
        validate_user_identifier(name.as_str()).map_err(ExternValidationError::Name)?;

        Ok(Self { name, signature })
    }
}

impl Quil for ExternDefinition {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        write!(writer, "{}", self.name)?;
        if let Some(signature) = &self.signature {
            write!(writer, " \"")?;
            signature.write(writer, fall_back_to_debug)?;
            write!(writer, "\"")?;
        }
        Ok(())
    }
}

/// An error that can occur when validating an extern definition.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ExternValidationError {
    /// The specified name is not a valid user identifier.
    #[error(transparent)]
    Name(#[from] IdentifierValidationError),
    /// There are more than one extern definitions with the same name.
    #[error("duplicate extern definition {0}")]
    Duplicate(String),
    /// The extern definition has a signature but it lacks a return or parameters.
    #[error("extern definition {0} has a signature but it lacks a return or parameters")]
    NoReturnOrParameters(String),
}

impl ExternDefinition {
    /// Validate a list of extern definitions from the same program. It validates the
    /// names, uniqueness, and the presence of return or parameters in the signature.
    pub(crate) fn validate_all(
        extern_definitions: &[ExternDefinition],
    ) -> Result<(), ExternValidationError> {
        extern_definitions
            .iter()
            .try_fold(HashSet::new(), |mut acc, extern_definition| {
                validate_user_identifier(extern_definition.name.as_str())
                    .map_err(ExternValidationError::Name)?;
                if acc.contains(&extern_definition.name) {
                    return Err(ExternValidationError::Duplicate(
                        extern_definition.name.clone(),
                    ));
                }
                if let Some(signature) = extern_definition.signature.as_ref() {
                    if !signature.has_return_or_parameters() {
                        return Err(ExternValidationError::NoReturnOrParameters(
                            extern_definition.name.clone(),
                        ));
                    }
                }
                acc.insert(extern_definition.name.clone());
                Ok(acc)
            })?;
        Ok(())
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
    /// the argument for a return parameter was invalid.
    #[error("return argument must be a memory reference, found {found:?}")]
    ReturnArgument { found: UnresolvedCallArgument },
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
    /// An immediate value. This may be resolved to a scalar.
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
/// the appropriate [`ExternSignature`]. It annotates the argument both with a type (and possibly
/// a length in the case of a vector) and whether it is mutable.
#[derive(Clone, Debug, PartialEq)]
pub enum ResolvedCallArgument {
    Vector {
        memory_region_name: String,
        vector: Vector,
        mutable: bool,
    },
    MemoryReference {
        memory_reference: MemoryReference,
        scalar_type: ScalarType,
        mutable: bool,
    },
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

impl Quil for ResolvedCallArgument {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        match self {
            ResolvedCallArgument::Vector {
                memory_region_name: value,
                ..
            } => write!(f, "{value}").map_err(Into::into),
            ResolvedCallArgument::MemoryReference {
                memory_reference: value,
                ..
            } => value.write(f, fall_back_to_debug),
            ResolvedCallArgument::Immediate { value, .. } => {
                write!(f, "{value}").map_err(Into::into)
            }
        }
    }
}

impl ResolvedCallArgument {
    /// Indicates whether the argument is mutable.
    pub(crate) fn is_mutable(&self) -> bool {
        match self {
            ResolvedCallArgument::Vector { mutable, .. } => *mutable,
            ResolvedCallArgument::MemoryReference { mutable, .. } => *mutable,
            ResolvedCallArgument::Immediate { .. } => false,
        }
    }

    /// Returns the name of the memory region. In the case of an immediate value,
    /// this will be `None`.
    pub(crate) fn name(&self) -> Option<String> {
        match self {
            ResolvedCallArgument::Vector {
                memory_region_name, ..
            } => Some(memory_region_name.clone()),
            ResolvedCallArgument::MemoryReference {
                memory_reference, ..
            } => Some(memory_reference.name.clone()),
            ResolvedCallArgument::Immediate { .. } => None,
        }
    }
}

/// A list of arguments for a call instruction. These may be resolved or unresolved.
/// To resolve a [`Call`] instruction, use [`crate::Program::resolve_call_instructions`].
#[derive(Clone, Debug, PartialEq, Hash, Eq)]
pub enum CallArguments {
    /// The resolved call arguments.
    Resolved(Vec<ResolvedCallArgument>),
    /// The unresolved call arguments.
    Unresolved(Vec<UnresolvedCallArgument>),
}

impl Quil for CallArguments {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        match self {
            CallArguments::Resolved(arguments) => {
                if !arguments.is_empty() {
                    write!(writer, " ")?;
                }
                for (i, argument) in arguments.iter().enumerate() {
                    argument.write(writer, fall_back_to_debug)?;
                    if i < arguments.len() - 1 {
                        write!(writer, " ")?;
                    }
                }
            }
            CallArguments::Unresolved(arguments) => {
                if !arguments.is_empty() {
                    write!(writer, " ")?;
                }
                for (i, argument) in arguments.iter().enumerate() {
                    argument.write(writer, fall_back_to_debug)?;
                    if i < arguments.len() - 1 {
                        write!(writer, " ")?;
                    }
                }
            }
        }
        Ok(())
    }
}

/// An error that can occur when validating a call instruction.
#[derive(Clone, Debug, PartialEq, thiserror::Error, Eq)]
pub enum CallValidationError {
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
    pub arguments: CallArguments,
}

impl Call {
    /// Create a new call instruction with resolved arguments. This will validate the
    /// name as a user identifier.
    pub fn try_new(
        name: String,
        arguments: Vec<UnresolvedCallArgument>,
    ) -> Result<Self, CallValidationError> {
        validate_user_identifier(name.as_str()).map_err(CallValidationError::Name)?;

        Ok(Self {
            name,
            arguments: CallArguments::Unresolved(arguments),
        })
    }
}

/// An error that can occur when resolving a call instruction.
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
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum CallSignatureError {
    #[error("expected {expected} arguments, found {found}")]
    ParameterCount { expected: usize, found: usize },
    #[error("error resolving arguments: {0:?}")]
    Arguments(Vec<CallArgumentError>),
}

/// An error that can occur when resolving a call instruction within the context
/// of a program.
#[derive(Debug, thiserror::Error, PartialEq)]
pub enum CallResolutionError {
    /// A matching extern instruction was found, but the signature validation failed.
    #[error("call found matching extern instruction for {name}, but signature validation failed: {error:?}")]
    Signature {
        name: String,
        error: CallSignatureError,
    },
    /// A matching extern instruction was found, but it has no signature.
    #[error("call found matching extern instruction for {0}, but it has no signature")]
    NoSignature(String),
    /// No matching extern instruction was found.
    #[error("no extern instruction found with name {0}")]
    NoMatchingExternInstruction(String),
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
        &mut self,
        signature: &ExternSignature,
        memory_regions: &IndexMap<String, MemoryRegion>,
    ) -> Result<(), CallSignatureError> {
        let mut expected_parameter_count = signature.parameters.len();
        if signature.return_type.is_some() {
            expected_parameter_count += 1;
        }
        let actual_parameter_count = match &self.arguments {
            CallArguments::Resolved(arguments) => arguments.len(),
            CallArguments::Unresolved(arguments) => arguments.len(),
        };
        if actual_parameter_count != expected_parameter_count {
            return Err(CallSignatureError::ParameterCount {
                expected: expected_parameter_count,
                found: actual_parameter_count,
            });
        }

        let resolved_call_arguments = match &self.arguments {
            CallArguments::Resolved(arguments) => {
                let unresolved_call_arguments = arguments
                    .iter()
                    .cloned()
                    .map(UnresolvedCallArgument::from)
                    .collect::<Vec<_>>();

                convert_unresolved_to_resolved_call_arguments(
                    unresolved_call_arguments.as_slice(),
                    signature,
                    memory_regions,
                )?
            }
            CallArguments::Unresolved(arguments) => {
                convert_unresolved_to_resolved_call_arguments(arguments, signature, memory_regions)?
            }
        };

        self.arguments = CallArguments::Resolved(resolved_call_arguments);
        Ok(())
    }

    /// Resolve the [`Call`] instruction to any of the given [`ExternSignature`] and memory regions.
    /// If no matching extern instruction is found, return an error.
    pub fn resolve(
        &mut self,
        memory_regions: &IndexMap<String, MemoryRegion>,
        extern_definitions: &[ExternDefinition],
    ) -> Result<(), CallResolutionError> {
        for definition in extern_definitions {
            if definition.name == self.name {
                let signature = definition
                    .signature
                    .as_ref()
                    .ok_or_else(|| CallResolutionError::NoSignature(self.name.clone()))?;
                return self
                    .resolve_to_signature(signature, memory_regions)
                    .map_err(|error| CallResolutionError::Signature {
                        name: self.name.clone(),
                        error,
                    });
            }
        }
        Err(CallResolutionError::NoMatchingExternInstruction(
            self.name.clone(),
        ))
    }
}

impl Quil for Call {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(f, "CALL {}", self.name)?;
        self.arguments.write(f, fall_back_to_debug)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    /// Test cases for the `ExternDefinition` Quil representation.
    struct ExternDefinitionQuilTestCase {
        /// The extern definition to test.
        definition: ExternDefinition,
        /// The expected Quil representation.
        expected: &'static str,
    }

    impl ExternDefinitionQuilTestCase {
        /// Signature with return and parameters
        fn case_01() -> Self {
            Self {
                definition: ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(ExternSignature {
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
                    }),
                },
                expected: "foo \"INTEGER (bar : INTEGER, baz : mut BIT[2])\"",
            }
        }

        /// Signature with only parameters
        fn case_02() -> Self {
            let definition = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
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
                }),
            };
            Self {
                definition,
                expected: "foo \"(bar : INTEGER, baz : mut BIT[2])\"",
            }
        }

        /// Signature with return only
        fn case_03() -> Self {
            let definition = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![],
                }),
            };
            Self {
                definition,
                expected: "foo \"INTEGER\"",
            }
        }

        /// Signature with no return nor parameters
        fn case_04() -> Self {
            let definition = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: None,
                    parameters: vec![],
                }),
            };
            Self {
                definition,
                expected: "foo \"\"",
            }
        }

        /// No signature
        fn case_05() -> Self {
            let definition = ExternDefinition {
                name: "foo".to_string(),
                signature: None,
            };
            Self {
                definition,
                expected: "foo",
            }
        }

        /// Variable length vector
        fn case_06() -> Self {
            let definition = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: None,
                    parameters: vec![ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::VariableLengthVector(ScalarType::Integer),
                    }],
                }),
            };
            Self {
                definition,
                expected: "foo \"(bar : INTEGER[])\"",
            }
        }
    }

    /// Test that the Quil representation of an `ExternDefinition` is as expected.
    #[rstest]
    #[case(ExternDefinitionQuilTestCase::case_01())]
    #[case(ExternDefinitionQuilTestCase::case_02())]
    #[case(ExternDefinitionQuilTestCase::case_03())]
    #[case(ExternDefinitionQuilTestCase::case_04())]
    #[case(ExternDefinitionQuilTestCase::case_05())]
    #[case(ExternDefinitionQuilTestCase::case_06())]
    fn test_extern_definition_quil(#[case] test_case: ExternDefinitionQuilTestCase) {
        assert_eq!(
            test_case
                .definition
                .to_quil()
                .expect("must be able to call to quil"),
            test_case.expected.to_string()
        );
    }

    /// Test cases for the `Call` Quil representation.
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
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "bar".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("baz".to_string()),
                ]),
            };
            Self {
                call,
                expected: "CALL foo bar[0] 2 baz",
            }
        }

        fn case_02() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "bar".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Identifier("baz".to_string()),
                ]),
            };
            Self {
                call,
                expected: "CALL foo bar[0] baz",
            }
        }

        fn case_03() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "bar".to_string(),
                        index: 0,
                    }),
                ]),
            };
            Self {
                call,
                expected: "CALL foo bar[0]",
            }
        }

        fn case_04() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![]),
            };

            Self {
                call,
                expected: "CALL foo",
            }
        }
    }

    /// Test that the Quil representation of a `Call` instruction is as expected.
    #[rstest]
    #[case(CallQuilTestCase::case_01())]
    #[case(CallQuilTestCase::case_02())]
    #[case(CallQuilTestCase::case_03())]
    #[case(CallQuilTestCase::case_04())]
    fn test_call_definition_quil(#[case] test_case: CallQuilTestCase) {
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

        /// Immediate value
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
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ]),
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
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ]),
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
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                ]),
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

        /// Already resolved is converted back to unresolved and re-resolved.
        fn case_04() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Resolved(vec![ResolvedCallArgument::MemoryReference {
                    memory_reference: MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    },
                    scalar_type: ScalarType::Integer,
                    mutable: true,
                }]),
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Integer),
                parameters: vec![],
            };
            Self {
                call: call.clone(),
                signature,
                expected: Ok(vec![ResolvedCallArgument::MemoryReference {
                    memory_reference: MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    },
                    scalar_type: ScalarType::Integer,
                    mutable: true,
                }]),
            }
        }

        /// Parameter count mismatch with return and parameters
        fn case_05() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ]),
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
        fn case_06() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                ]),
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
        fn case_07() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ]),
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
        fn case_08() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::Immediate(Complex64::new(2.0, 0.0)),
                    UnresolvedCallArgument::Identifier("bit".to_string()),
                ]),
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
    #[case(ResolveToSignatureTestCase::case_08())]
    fn test_assert_matching_signature(#[case] mut test_case: ResolveToSignatureTestCase) {
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

    /// Test cases for call resolution against a set of extern definitions.
    struct CallResolutionTestCase {
        /// The call instruction to resolve.
        call: Call,
        /// The set of extern definitions to resolve against.
        extern_definitions: Vec<ExternDefinition>,
        /// The expected result of the resolution.
        expected: Result<Vec<ResolvedCallArgument>, CallResolutionError>,
    }

    impl CallResolutionTestCase {
        /// Valid resolution
        fn case_01() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                ]),
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
                extern_definitions: vec![ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(signature),
                }],
                expected: Ok(resolved),
            }
        }

        /// Signature does not match
        fn case_02() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                ]),
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Real),
                parameters: vec![],
            };
            Self {
                call,
                extern_definitions: vec![ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(signature),
                }],
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

        /// No signature on extern definition
        fn case_03() -> Self {
            let call = Call {
                name: "foo".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                ]),
            };
            Self {
                call,
                extern_definitions: vec![ExternDefinition {
                    name: "foo".to_string(),
                    signature: None,
                }],
                expected: Err(CallResolutionError::NoSignature("foo".to_string())),
            }
        }

        /// No corresponding extern definition
        fn case_04() -> Self {
            let call = Call {
                name: "undeclared".to_string(),
                arguments: CallArguments::Unresolved(vec![
                    UnresolvedCallArgument::MemoryReference(MemoryReference {
                        name: "integer".to_string(),
                        index: 0,
                    }),
                ]),
            };
            let signature = ExternSignature {
                return_type: Some(ScalarType::Real),
                parameters: vec![],
            };
            Self {
                call,
                extern_definitions: vec![ExternDefinition {
                    name: "foo".to_string(),
                    signature: Some(signature),
                }],
                expected: Err(CallResolutionError::NoMatchingExternInstruction(
                    "undeclared".to_string(),
                )),
            }
        }
    }

    /// Test resolution of `Call` instructions against a set of extern definitions.
    #[rstest]
    #[case(CallResolutionTestCase::case_01())]
    #[case(CallResolutionTestCase::case_02())]
    #[case(CallResolutionTestCase::case_03())]
    #[case(CallResolutionTestCase::case_04())]
    fn test_call_resolution(#[case] mut test_case: CallResolutionTestCase) {
        let memory_regions = build_declarations();
        let found = test_case
            .call
            .resolve(&memory_regions, &test_case.extern_definitions);
        match (test_case.expected, found) {
            (Ok(expected), Ok(_)) => {
                assert_eq!(CallArguments::Resolved(expected), test_case.call.arguments)
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

    /// Test cases for validating extern definitions.
    struct ExternDefinitionTestCase {
        /// The set of extern definitions to validate.
        definitions: Vec<ExternDefinition>,
        /// The expected result of the validation.
        expected: Result<(), ExternValidationError>,
    }

    impl ExternDefinitionTestCase {
        /// Valid definitions
        fn case_01() -> Self {
            let definition1 = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Integer),
                    }],
                }),
            };
            let definition2 = ExternDefinition {
                name: "baz".to_string(),
                signature: Some(ExternSignature {
                    return_type: Some(ScalarType::Real),
                    parameters: vec![ExternParameter {
                        name: "biz".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Real),
                    }],
                }),
            };
            let definitions = vec![definition1, definition2];
            let expected = Ok(());
            Self {
                definitions,
                expected,
            }
        }

        /// Duplicate
        fn case_02() -> Self {
            let definition1 = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: Some(ScalarType::Integer),
                    parameters: vec![ExternParameter {
                        name: "bar".to_string(),
                        mutable: false,
                        data_type: ExternParameterType::Scalar(ScalarType::Integer),
                    }],
                }),
            };
            let definition2 = definition1.clone();
            let definitions = vec![definition1, definition2];
            let expected = Err(ExternValidationError::Duplicate("foo".to_string()));
            Self {
                definitions,
                expected,
            }
        }

        /// No return nor parameters
        fn case_03() -> Self {
            let definition1 = ExternDefinition {
                name: "foo".to_string(),
                signature: Some(ExternSignature {
                    return_type: None,
                    parameters: vec![],
                }),
            };
            let definitions = vec![definition1];
            let expected = Err(ExternValidationError::NoReturnOrParameters(
                "foo".to_string(),
            ));
            Self {
                definitions,
                expected,
            }
        }
    }

    /// Test validation of extern definitions.
    #[rstest]
    #[case(ExternDefinitionTestCase::case_01())]
    #[case(ExternDefinitionTestCase::case_02())]
    #[case(ExternDefinitionTestCase::case_03())]
    fn test_extern_definition_validation(#[case] test_case: ExternDefinitionTestCase) {
        let found = ExternDefinition::validate_all(test_case.definitions.as_slice());
        match (test_case.expected, found) {
            (Ok(_), Ok(_)) => {}
            (Ok(_), Err(found)) => {
                panic!("expected valid, found err {:?}", found)
            }
            (Err(expected), Ok(_)) => {
                panic!("expected err {:?}, found valid", expected)
            }
            (Err(expected), Err(found)) => assert_eq!(expected, found),
        }
    }
}
