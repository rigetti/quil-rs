use std::str::FromStr;

use indexmap::IndexMap;
use nom_locate::LocatedSpan;

#[cfg(feature = "stubs")]
use pyo3_stub_gen::derive::{gen_stub_pyclass, gen_stub_pyclass_complex_enum};

use super::{MemoryReference, Qubit, QuotedString, WaveformInvocation};
use crate::{
    expression::Expression,
    parser::{common::parse_frame_identifier, lex, ParseError},
    pickleable_new,
    program::{disallow_leftover, SyntaxError},
    quil::{Quil, INDENT},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, strum::EnumTryAs)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass_complex_enum)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        from_py_object
    )
)]
pub enum AttributeValue {
    String(String),
    Expression(Expression),
}

impl Quil for AttributeValue {
    fn write(
        &self,
        f: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        use AttributeValue::*;
        match self {
            String(value) => write!(f, "{}", QuotedString(value)).map_err(Into::into),
            Expression(value) => value.write(f, fall_back_to_debug),
        }
    }
}

#[cfg(test)]
impl proptest::arbitrary::Arbitrary for AttributeValue {
    type Parameters = ();
    type Strategy = proptest::prelude::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use crate::expression::proptest_helpers::arb_name;
        use proptest::prelude::*;

        proptest_helpers::arb_attribute_value(arb_name(), any::<Expression>()).boxed()
    }
}

pub type FrameAttributes = IndexMap<String, AttributeValue>;

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        get_all,
        set_all,
        subclass,
        from_py_object
    )
)]
pub struct FrameDefinition {
    pub identifier: FrameIdentifier,
    pub attributes: FrameAttributes,
}

pickleable_new! {
    impl FrameDefinition {
        pub fn new(identifier: FrameIdentifier, attributes: FrameAttributes);
    }
}

impl Quil for FrameDefinition {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "DEFFRAME ")?;
        self.identifier.write(writer, fall_back_to_debug)?;
        write!(writer, ":")?;
        for (key, value) in &self.attributes {
            write!(writer, "\n{INDENT}{key}: ")?;
            value.write(writer, fall_back_to_debug)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct FrameIdentifier {
    pub name: String,
    pub qubits: Vec<Qubit>,
}

pickleable_new! {
    impl FrameIdentifier {
        pub fn new(name: String, qubits: Vec<Qubit>);
    }
}

impl Quil for FrameIdentifier {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> std::result::Result<(), crate::quil::ToQuilError> {
        for qubit in &self.qubits {
            qubit.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
        }
        write!(writer, "{}", QuotedString(&self.name)).map_err(Into::into)
    }
}

impl FromStr for FrameIdentifier {
    type Err = SyntaxError<Self>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = LocatedSpan::new(s);
        let tokens = lex(input)?;
        disallow_leftover(
            parse_frame_identifier(&tokens).map_err(ParseError::from_nom_internal_err),
        )
    }
}

#[cfg(test)]
impl proptest::arbitrary::Arbitrary for FrameIdentifier {
    type Parameters = ();
    type Strategy = proptest::prelude::BoxedStrategy<Self>;

    fn arbitrary_with(_args: Self::Parameters) -> Self::Strategy {
        use crate::expression::proptest_helpers::arb_name;
        use proptest::prelude::*;

        proptest_helpers::arb_frame_identifier(arb_name(), any::<u64>(), arb_name()).boxed()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        get_all,
        set_all,
        subclass,
        from_py_object
    )
)]
pub struct Capture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub memory_reference: MemoryReference,
    pub waveform: WaveformInvocation,
}

pickleable_new! {
    impl Capture {
        pub fn new(
            blocking: bool,
            frame: FrameIdentifier,
            memory_reference: MemoryReference,
            waveform: WaveformInvocation,
        );
    }
}

impl Quil for Capture {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> Result<(), crate::quil::ToQuilError> {
        {
            if self.blocking {
                write!(writer, "CAPTURE ")?;
            } else {
                write!(writer, "NONBLOCKING CAPTURE ")?;
            }

            self.frame.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
            self.waveform.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
            self.memory_reference.write(writer, fall_back_to_debug)?;
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        get_all,
        set_all,
        subclass,
        from_py_object
    )
)]
pub struct Pulse {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub waveform: WaveformInvocation,
}

pickleable_new! {
    impl Pulse {
        pub fn new(blocking: bool, frame: FrameIdentifier, waveform: WaveformInvocation);
    }
}

impl Quil for Pulse {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        {
            if self.blocking {
                write!(writer, "PULSE ")?;
            } else {
                write!(writer, "NONBLOCKING PULSE ")?;
            }
            self.frame.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
            self.waveform.write(writer, fall_back_to_debug)?;
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct RawCapture {
    pub blocking: bool,
    pub frame: FrameIdentifier,
    pub duration: Expression,
    pub memory_reference: MemoryReference,
}

pickleable_new! {
    impl RawCapture {
        pub fn new(
            blocking: bool,
            frame: FrameIdentifier,
            duration: Expression,
            memory_reference: MemoryReference,
        );
    }
}

impl Quil for RawCapture {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        {
            if self.blocking {
                write!(writer, "RAW-CAPTURE ")?;
            } else {
                write!(writer, "NONBLOCKING RAW-CAPTURE ")?;
            }
            self.frame.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
            self.duration.write(writer, fall_back_to_debug)?;
            write!(writer, " ")?;
            self.memory_reference.write(writer, fall_back_to_debug)?;
            Ok(())
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct SetFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

pickleable_new! {
    impl SetFrequency {
        pub fn new(frame: FrameIdentifier, frequency: Expression);
    }
}

impl Quil for SetFrequency {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SET-FREQUENCY ")?;
        self.frame.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.frequency.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct SetPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

pickleable_new! {
    impl SetPhase {
        pub fn new(frame: FrameIdentifier, phase: Expression);
    }
}

impl Quil for SetPhase {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SET-PHASE ")?;
        self.frame.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.phase.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct SetScale {
    pub frame: FrameIdentifier,
    pub scale: Expression,
}

pickleable_new! {
    impl SetScale {
        pub fn new(frame: FrameIdentifier, scale: Expression);
    }
}

impl Quil for SetScale {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SET-SCALE ")?;
        self.frame.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.scale.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct ShiftFrequency {
    pub frame: FrameIdentifier,
    pub frequency: Expression,
}

pickleable_new! {
    impl ShiftFrequency {
        pub fn new(frame: FrameIdentifier, frequency: Expression);
    }
}

impl Quil for ShiftFrequency {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SHIFT-FREQUENCY ")?;
        self.frame.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.frequency.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct ShiftPhase {
    pub frame: FrameIdentifier,
    pub phase: Expression,
}

pickleable_new! {
    impl ShiftPhase {
        pub fn new(frame: FrameIdentifier, phase: Expression);
    }
}

impl Quil for ShiftPhase {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SHIFT-PHASE ")?;
        self.frame.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.phase.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "stubs", gen_stub_pyclass)]
#[cfg_attr(
    feature = "python",
    pyo3::pyclass(
        module = "quil._quil.instructions",
        eq,
        frozen,
        hash,
        get_all,
        subclass,
        from_py_object
    )
)]
pub struct SwapPhases {
    pub frame_1: FrameIdentifier,
    pub frame_2: FrameIdentifier,
}

pickleable_new! {
    impl SwapPhases {
        pub fn new(frame_1: FrameIdentifier, frame_2: FrameIdentifier);
    }
}

impl Quil for SwapPhases {
    fn write(
        &self,
        writer: &mut impl std::fmt::Write,
        fall_back_to_debug: bool,
    ) -> crate::quil::ToQuilResult<()> {
        write!(writer, "SWAP-PHASES ")?;
        self.frame_1.write(writer, fall_back_to_debug)?;
        write!(writer, " ")?;
        self.frame_2.write(writer, fall_back_to_debug)?;
        Ok(())
    }
}

#[cfg(test)]
pub mod proptest_helpers {
    use super::*;

    use proptest::{prelude::*, sample::SizeRange};

    use crate::instruction::QubitPlaceholder;

    prop_compose! {
        pub fn arb_frame_identifier(
            arb_name: impl Strategy<Value = String>,
            arb_fixed: impl Strategy<Value = u64>,
            arb_variable: impl Strategy<Value = String>
        )
        (
            name in arb_name,
            qubits in proptest::collection::vec(arb_qubit(arb_fixed, arb_variable), SizeRange::default())
        )
        -> FrameIdentifier {
            FrameIdentifier {
                name,
                qubits,
            }
        }
    }

    pub fn arb_qubit(
        arb_fixed: impl Strategy<Value = u64>,
        arb_variable: impl Strategy<Value = String>,
    ) -> impl Strategy<Value = Qubit> {
        prop_oneof![
            arb_fixed.prop_map(Qubit::Fixed),
            arb_variable.prop_map(Qubit::Variable),
            Just(QubitPlaceholder::default()).prop_map(Qubit::Placeholder),
        ]
    }

    pub fn arb_attribute_value(
        arb_string: impl Strategy<Value = String>,
        arb_expr: impl Strategy<Value = Expression>,
    ) -> impl Strategy<Value = AttributeValue> {
        prop_oneof![
            arb_string.prop_map(AttributeValue::String),
            arb_expr.prop_map(AttributeValue::Expression),
        ]
    }
}
