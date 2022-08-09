//! Checks for consistency and validity of Quil programs.
//!
//! See the [Quil spec](https://quil-lang.github.io/).
use crate::{
    expression::Expression,
    instruction::{
        Arithmetic, ArithmeticOperand, ArithmeticOperator, BinaryLogic, BinaryOperand,
        BinaryOperator, Comparison, ComparisonOperand, ComparisonOperator, Exchange, Instruction,
        Load, MemoryReference, Move, ScalarType, SetFrequency, SetPhase, SetScale, ShiftFrequency,
        ShiftPhase, Store, UnaryLogic, UnaryOperator,
    },
    program::{
        error::{ProgramError, ProgramResult},
        MemoryRegion,
    },
    Program,
};
use std::collections::BTreeMap;
use std::fmt::Debug;

/// Check that the instructions of the given program obey the spec with regards to data types.
///
/// See the [Quil spec](https://quil-lang.github.io/).
pub fn type_check(program: &Program) -> ProgramResult<()> {
    for instruction in &program.instructions {
        match instruction {
            Instruction::SetFrequency(SetFrequency { frequency, .. }) => {
                should_be_real(instruction, frequency, &program.memory_regions)?
            }
            Instruction::SetPhase(SetPhase { phase, .. }) => {
                should_be_real(instruction, phase, &program.memory_regions)?
            }
            Instruction::SetScale(SetScale { scale, .. }) => {
                should_be_real(instruction, scale, &program.memory_regions)?
            }
            Instruction::ShiftFrequency(ShiftFrequency { frequency, .. }) => {
                should_be_real(instruction, frequency, &program.memory_regions)?
            }
            Instruction::ShiftPhase(ShiftPhase { phase, .. }) => {
                should_be_real(instruction, phase, &program.memory_regions)?
            }
            Instruction::Arithmetic(Arithmetic {
                operator,
                destination,
                source,
            }) => type_check_arithmetic(
                instruction,
                operator,
                destination,
                source,
                &program.memory_regions,
            )?,
            Instruction::Comparison(Comparison { operator, operands }) => {
                type_check_comparison(instruction, operator, operands, &program.memory_regions)?
            }
            Instruction::BinaryLogic(BinaryLogic { operator, operands }) => {
                type_check_binary_logic(instruction, operator, operands, &program.memory_regions)?
            }
            Instruction::UnaryLogic(UnaryLogic { operator, operand }) => {
                type_check_unary_logic(instruction, operator, operand, &program.memory_regions)?
            }
            Instruction::Move(Move {
                destination,
                source,
            }) => type_check_move(instruction, destination, source, &program.memory_regions)?,
            Instruction::Exchange(Exchange { left, right }) => {
                type_check_exchange(instruction, left, right, &program.memory_regions)?
            }
            Instruction::Load(Load {
                destination,
                source,
                offset,
            }) => type_check_load(
                instruction,
                destination,
                source,
                offset,
                &program.memory_regions,
            )?,
            Instruction::Store(Store {
                destination,
                offset,
                source,
            }) => type_check_store(
                instruction,
                destination,
                offset,
                source,
                &program.memory_regions,
            )?,
            _ => {}
        }
    }
    Ok(())
}

fn undefined_memory_reference(
    instruction: &Instruction,
    reference: impl Debug,
) -> ProgramResult<()> {
    Err(ProgramError::UndefinedMemoryReference {
        instruction: instruction.clone(),
        reference: format!("{reference:#?}"),
    })
}

fn data_type_mismatch(
    instruction: &Instruction,
    dst: impl Debug,
    dst_type: impl Debug,
    src: impl Debug,
    src_type: impl Debug,
) -> ProgramResult<()> {
    Err(ProgramError::DataTypeMismatch {
        instruction: instruction.clone(),
        dst: format!("{dst:#?}"),
        dst_type: format!("{dst_type:#?}"),
        src: format!("{src:#?}"),
        src_type: format!("{src_type:#?}"),
    })
}

fn real_value_required(
    instruction: &Instruction,
    value: impl Debug,
    data_type: impl Debug,
) -> ProgramResult<()> {
    Err(ProgramError::RealValueRequired {
        instruction: instruction.clone(),
        value: format!("{value:#?}"),
        data_type: format!("#{data_type:#?}"),
    })
}

fn operator_operand_mismatch(
    instruction: &Instruction,
    operator: impl Debug,
    correct_type: impl Debug,
    operand: impl Debug,
    data_type: impl Debug,
) -> ProgramResult<()> {
    Err(ProgramError::OperatorOperandMismatch {
        instruction: instruction.clone(),
        operator: format!("{operator:#?}"),
        correct_type: format!("{correct_type:#?}"),
        operand: format!("{operand:#?}"),
        data_type: format!("{data_type:#?}"),
    })
}

fn should_be_real(
    instruction: &Instruction,
    this_expression: &Expression,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match this_expression {
        Expression::Address(reference) => {
            if let Some(MemoryRegion { size, .. }) = memory_regions.get(&reference.name) {
                let dt = &size.data_type;
                if dt == &ScalarType::Real {
                    Ok(())
                } else {
                    real_value_required(instruction, reference, dt)
                }
            } else {
                undefined_memory_reference(instruction, reference)
            }
        }
        Expression::FunctionCall { expression, .. } => {
            should_be_real(instruction, expression, memory_regions)
        }
        Expression::Infix { left, right, .. } => should_be_real(instruction, left, memory_regions)
            .and(should_be_real(instruction, right, memory_regions)),
        Expression::Number(value) => {
            if value.im.abs() > f64::EPSILON {
                real_value_required(instruction, this_expression, "`imaginary`")
            } else {
                Ok(())
            }
        }
        Expression::PiConstant => Ok(()),
        Expression::Prefix { expression, .. } => {
            should_be_real(instruction, expression, memory_regions)
        }
        Expression::Variable(_) => real_value_required(instruction, this_expression, "`variable`"),
    }
}

fn type_check_arithmetic(
    instruction: &Instruction,
    operator: &ArithmeticOperator,
    destination: &ArithmeticOperand,
    source: &ArithmeticOperand,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match destination {
        ArithmeticOperand::LiteralInteger(_) | ArithmeticOperand::LiteralReal(_) => {
            operator_operand_mismatch(
                instruction,
                "operation",
                "memory reference",
                destination,
                "literal value",
            )
        }
        ArithmeticOperand::MemoryReference(dest_ref) => {
            if let Some(dest_region) = memory_regions.get(&dest_ref.name) {
                let dt = &dest_region.size.data_type;
                match (source, dt) {
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Integer) => Ok(()),
                    (ArithmeticOperand::LiteralReal(_), ScalarType::Real) => Ok(()),
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Real) => {
                        data_type_mismatch(instruction, dest_ref, dt, source, "`literal integer`")
                    }
                    (ArithmeticOperand::LiteralReal(_), _) => {
                        data_type_mismatch(instruction, dest_ref, dt, source, "`literal real`")
                    }
                    (_, ScalarType::Bit) | (_, ScalarType::Octet) => operator_operand_mismatch(
                        instruction,
                        operator,
                        "real or integral-valued",
                        dest_ref,
                        dt,
                    ),
                    (ArithmeticOperand::MemoryReference(src_ref), _) => {
                        if let Some(src_region) = memory_regions.get(&src_ref.name) {
                            let st = &src_region.size.data_type;
                            match st {
                                ScalarType::Bit | ScalarType::Octet => operator_operand_mismatch(
                                    instruction,
                                    operator,
                                    "real or integral-valued",
                                    src_ref,
                                    st,
                                ),
                                st if dt != st => {
                                    data_type_mismatch(instruction, dest_ref, dt, src_ref, st)
                                }
                                _ => Ok(()),
                            }
                        } else {
                            undefined_memory_reference(instruction, src_ref)
                        }
                    }
                }
            } else {
                undefined_memory_reference(instruction, dest_ref)
            }
        }
    }
}

fn type_check_comparison(
    instruction: &Instruction,
    operator: &ComparisonOperator,
    operands: &(MemoryReference, MemoryReference, ComparisonOperand),
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    let (x, y, z) = operands;
    match (memory_regions.get(&x.name), memory_regions.get(&y.name)) {
        (None, _) => undefined_memory_reference(instruction, x),
        (_, None) => undefined_memory_reference(instruction, y),
        (Some(x_region), Some(y_region)) => {
            let (xt, yt) = (&x_region.size.data_type, &y_region.size.data_type);
            if xt != &ScalarType::Bit {
                operator_operand_mismatch(instruction, operator, "bit", x, xt)
            } else {
                match (yt, z) {
                    (ScalarType::Real, ComparisonOperand::LiteralInteger(_)) => {
                        data_type_mismatch(instruction, y, yt, z, "`literal integer`")
                    }
                    (_, ComparisonOperand::LiteralReal(_)) if yt != &ScalarType::Real => {
                        data_type_mismatch(instruction, y, yt, z, "`literal real`")
                    }
                    (_, ComparisonOperand::MemoryReference(z_ref)) => {
                        if let Some(z_region) = memory_regions.get(&z_ref.name) {
                            let zt = &z_region.size.data_type;
                            if yt != zt {
                                data_type_mismatch(instruction, y, yt, z, zt)
                            } else {
                                Ok(())
                            }
                        } else {
                            undefined_memory_reference(instruction, z_ref)
                        }
                    }
                    _ => Ok(()),
                }
            }
        }
    }
}

fn type_check_binary_logic(
    instruction: &Instruction,
    operator: &BinaryOperator,
    operands: &(MemoryReference, BinaryOperand),
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    let (x, y) = operands;
    if let Some(x_region) = memory_regions.get(&x.name) {
        let xt = &x_region.size.data_type;
        if xt == &ScalarType::Real {
            operator_operand_mismatch(instruction, operator, "integral", x, xt)
        } else {
            match y {
                BinaryOperand::LiteralInteger(_) => Ok(()),
                BinaryOperand::MemoryReference(y_ref) => {
                    if let Some(y_region) = memory_regions.get(&y_ref.name) {
                        let yt = &y_region.size.data_type;
                        if yt == &ScalarType::Real {
                            operator_operand_mismatch(instruction, operator, "integral", y, yt)
                        } else {
                            Ok(())
                        }
                    } else {
                        undefined_memory_reference(instruction, y_ref)
                    }
                }
            }
        }
    } else {
        undefined_memory_reference(instruction, x)
    }
}

fn type_check_unary_logic(
    instruction: &Instruction,
    operator: &UnaryOperator,
    operand: &MemoryReference,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    if let Some(MemoryRegion { size, .. }) = memory_regions.get(&operand.name) {
        let dt = &size.data_type;
        match (dt, operator) {
            (ScalarType::Real, UnaryOperator::Not) => {
                operator_operand_mismatch(instruction, operator, "integral", operand, dt)
            }
            (ScalarType::Bit, UnaryOperator::Neg) | (ScalarType::Octet, UnaryOperator::Neg) => {
                operator_operand_mismatch(
                    instruction,
                    operator,
                    "real or integral-valued",
                    operand,
                    dt,
                )
            }
            (ScalarType::Real, UnaryOperator::Neg) | (ScalarType::Integer, UnaryOperator::Neg) => {
                Ok(())
            }
            (ScalarType::Integer, UnaryOperator::Not)
            | (ScalarType::Bit, UnaryOperator::Not)
            | (ScalarType::Octet, UnaryOperator::Not) => Ok(()),
        }
    } else {
        undefined_memory_reference(instruction, operand)
    }
}

fn type_check_move(
    instruction: &Instruction,
    destination: &ArithmeticOperand,
    source: &ArithmeticOperand,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match destination {
        ArithmeticOperand::LiteralInteger(_) | ArithmeticOperand::LiteralReal(_) => {
            operator_operand_mismatch(
                instruction,
                "MOVE",
                "memory reference",
                destination,
                "literal value",
            )
        }
        ArithmeticOperand::MemoryReference(dest_ref) => {
            if let Some(dest_region) = memory_regions.get(&dest_ref.name) {
                let dt = &dest_region.size.data_type;
                match (source, dt) {
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Real) => data_type_mismatch(
                        instruction,
                        destination,
                        dt,
                        source,
                        "`literal integer`",
                    ),
                    (ArithmeticOperand::LiteralReal(_), st) if st != &ScalarType::Real => {
                        data_type_mismatch(instruction, destination, dt, source, "`literal real`")
                    }
                    (ArithmeticOperand::MemoryReference(src_ref), dt) => {
                        if let Some(src_region) = memory_regions.get(&src_ref.name) {
                            let st = &src_region.size.data_type;
                            if st != dt {
                                data_type_mismatch(instruction, destination, dt, source, st)
                            } else {
                                Ok(())
                            }
                        } else {
                            undefined_memory_reference(instruction, src_ref)
                        }
                    }
                    _ => Ok(()),
                }
            } else {
                undefined_memory_reference(instruction, dest_ref)
            }
        }
    }
}

fn type_check_exchange(
    instruction: &Instruction,
    left: &ArithmeticOperand,
    right: &ArithmeticOperand,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match (left, right) {
        (
            ArithmeticOperand::MemoryReference(left_ref),
            ArithmeticOperand::MemoryReference(right_ref),
        ) => {
            match (
                memory_regions.get(&left_ref.name),
                memory_regions.get(&right_ref.name),
            ) {
                (None, _) => undefined_memory_reference(instruction, left_ref),
                (_, None) => undefined_memory_reference(instruction, right_ref),
                (Some(left_region), Some(right_region)) => {
                    let (lt, rt) = (&left_region.size.data_type, &right_region.size.data_type);
                    if lt != rt {
                        data_type_mismatch(instruction, left, lt, right, rt)
                    } else {
                        Ok(())
                    }
                }
            }
        }
        (ArithmeticOperand::LiteralInteger(_), _) | (ArithmeticOperand::LiteralReal(_), _) => {
            operator_operand_mismatch(
                instruction,
                "EXCHANGE",
                "memory reference",
                left,
                "literal value",
            )
        }
        (_, ArithmeticOperand::LiteralInteger(_)) | (_, ArithmeticOperand::LiteralReal(_)) => {
            operator_operand_mismatch(
                instruction,
                "EXCHANGE",
                "memory reference",
                right,
                "literal value",
            )
        }
    }
}

fn type_check_load(
    instruction: &Instruction,
    destination: &MemoryReference,
    source: &str,
    offset: &MemoryReference,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match (
        memory_regions.get(&destination.name),
        memory_regions.get(source),
        memory_regions.get(&offset.name),
    ) {
        (None, _, _) => undefined_memory_reference(instruction, destination),
        (_, None, _) => undefined_memory_reference(instruction, source),
        (_, _, None) => undefined_memory_reference(instruction, offset),
        (Some(dest_region), Some(src_region), Some(offset_region)) => {
            let (dt, st, ot) = (
                &dest_region.size.data_type,
                &src_region.size.data_type,
                &offset_region.size.data_type,
            );
            if ot != &ScalarType::Integer {
                operator_operand_mismatch(instruction, "LOAD", "integral", offset, ot)
            } else if dt != st {
                data_type_mismatch(instruction, destination, dt, source, st)
            } else {
                Ok(())
            }
        }
    }
}

fn type_check_store(
    instruction: &Instruction,
    destination: &str,
    offset: &MemoryReference,
    source: &ArithmeticOperand,
    memory_regions: &BTreeMap<String, MemoryRegion>,
) -> ProgramResult<()> {
    match (
        memory_regions.get(destination),
        memory_regions.get(&offset.name),
    ) {
        (None, _) => undefined_memory_reference(instruction, destination),
        (_, None) => undefined_memory_reference(instruction, offset),
        (Some(dest_region), Some(offset_region)) => {
            let (dt, ot) = (&dest_region.size.data_type, &offset_region.size.data_type);
            if ot != &ScalarType::Integer {
                operator_operand_mismatch(instruction, "STORE", "integral", offset, ot)
            } else {
                // https://quil-lang.github.io/#6-5Classical-Instructions
                // # Perform an indirect store of a to x offset by n.
                // STORE    x n a          # x[n] := a
                //      <oct*> <int> <oct>
                //      <oct*> <int> <!int>
                //      <int*> <int> <int>
                //      <int*> <int> <!int>
                //      <real*> <int> <real>
                //      <real*> <int> <!real>
                //      <bit*> <int> <bit>
                //      <bit*> <int> <!int>
                match (source, dt) {
                    // <oct*> <int> <!int>
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Octet) => Ok(()),
                    // <int*> <int> <!int>
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Integer) => Ok(()),
                    // <real*> <int> <!real>
                    (ArithmeticOperand::LiteralReal(_), ScalarType::Real) => Ok(()),
                    // <bit*> <int> <!bit>
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Bit) => Ok(()),
                    // <d*> <int> <s>  => check that d & s match
                    (ArithmeticOperand::MemoryReference(src_ref), _) => {
                        if let Some(src_region) = memory_regions.get(&src_ref.name) {
                            let st = &src_region.size.data_type;
                            if st == dt {
                                Ok(())
                            } else {
                                data_type_mismatch(instruction, src_ref, st, destination, dt)
                            }
                        } else {
                            undefined_memory_reference(instruction, src_ref)
                        }
                    }
                    // Mismatches
                    (ArithmeticOperand::LiteralInteger(_), ScalarType::Real) => data_type_mismatch(
                        instruction,
                        source,
                        "`literal integer`",
                        destination,
                        dt,
                    ),
                    (ArithmeticOperand::LiteralReal(_), ScalarType::Integer) => {
                        data_type_mismatch(instruction, source, "`literal real`", destination, dt)
                    }
                    (ArithmeticOperand::LiteralReal(_), ScalarType::Bit) => {
                        data_type_mismatch(instruction, source, "`literal real`", destination, dt)
                    }
                    (ArithmeticOperand::LiteralReal(_), ScalarType::Octet) => {
                        data_type_mismatch(instruction, source, "`literal real`", destination, dt)
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Program;
    use rstest::*;
    use std::str::FromStr;

    #[rstest]
    fn test_sets_and_shifts(
        #[values("x", "y")] declared: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] datatype: &str,
        #[values(
            "SET-FREQUENCY",
            "SET-PHASE",
            "SET-SCALE",
            "SHIFT-FREQUENCY",
            "SHIFT-PHASE"
        )]
        command: &str,
        #[values("x", "y")] referenced: &str,
    ) {
        let p = Program::from_str(&format!(
            r#"
DECLARE {declared} {datatype}
{command} 0 "xy" {referenced}
"#
        )).expect(&format!(
            "Bad program with (declared, datatype, command, referenced) = ({declared}, {datatype}, {command}, {referenced})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            declared == referenced && datatype == "REAL"
        );
    }

    #[rstest]
    fn test_arithmetic_memory_references(
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] src_type: &str,
        #[values("ADD", "SUB", "MUL", "DIV")] command: &str,
    ) {
        let p = Program::from_str(&format!(
            r#"
DECLARE destination {dst_type}
DECLARE source {src_type}
{command} destination source
"#
        )).expect(&format!(
            "Bad arithmetic program with (command, dst_type, src_type) = ({command}, {dst_type}, {src_type})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type == src_type && ["REAL", "INTEGER"].contains(&dst_type)
        );
    }

    #[rstest]
    fn test_arithmetic_immediate_values(
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("ADD", "SUB", "MUL", "DIV")] command: &str,
        #[values("1", "1.0")] value: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE destination {dst_type}
{command} destination {value}
"#
        )).expect(&format!(
            "Bad ARITHMETIC program with (command, dst_type, value) = ({command}, {dst_type}, {value})."
        ));
        let (f, i) = (f64::from_str(value), i64::from_str(value));
        dbg!((&p, &f, &i));
        assert_eq!(
            type_check(&p).is_ok(),
            (dst_type == "REAL" && f.is_ok() && i.is_err()) || (dst_type == "INTEGER" && i.is_ok())
        );
    }

    #[rstest]
    fn test_comparison_memory_references(
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] left_type: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] right_type: &str,
        #[values("EQ", "GT", "GE", "LT", "LE")] comparison: &str,
    ) {
        let p = Program::from_str(&format!(
                                r#"
DECLARE destination {dst_type}
DECLARE left {left_type}
DECLARE right {right_type}
{comparison} destination left right
"#
        )).expect(&format!(
            "Bad comparison program with (dst_type, left_type, right_type, comparison) = ({dst_type}, {left_type}, {right_type}, {comparison})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type == "BIT" && left_type == right_type
        );
    }

    #[rstest]
    fn test_comparison_immediate_values(
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] left_type: &str,
        #[values("EQ", "GT", "GE", "LT", "LE")] comparison: &str,
        #[values("1.0", "1")] value: &str,
    ) {
        let p = Program::from_str(&format!(
                                r#"
DECLARE destination {dst_type}
DECLARE left {left_type}
{comparison} destination left {value}
"#
        )).expect(&format!(
            "Bad comparison program with (dst_type, left_type, comparison, value) = ({dst_type}, {left_type}, {comparison}, {value})."
        ));
        let (f, i) = (f64::from_str(value), i64::from_str(value));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type == "BIT"
                && ((left_type == "REAL" && f.is_ok() && i.is_err())
                    || (left_type != "REAL" && i.is_ok()))
        );
    }

    #[rstest]
    fn test_binary_logic_memory_references(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("y")] src_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] src_type: &str,
        #[values("AND", "IOR", "XOR")] operator: &str,
        #[values("x", "not_x")] dst_ref: &str,
        #[values("y", "not_y")] src_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {dst_decl} {dst_type}
DECLARE {src_decl} {src_type}
{operator} {dst_ref} {src_ref}
"#
        )).expect(&format!(
            "Bad bianry logic program with (dst_decl, dst_type, src_decl, src_type, operator, dst_ref, src_ref) = ({dst_decl}, {dst_type}, {src_decl}, {src_type}, {operator}, {dst_ref}, {src_ref})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type != "REAL" && src_type != "REAL" && dst_decl == dst_ref && src_decl == src_ref
        );
    }

    #[rstest]
    fn test_binary_logic_immediate_values(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("AND", "IOR", "XOR")] operator: &str,
        #[values("x", "not_x")] dst_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {dst_decl} {dst_type}
{operator} {dst_ref} 1
"#
            )).expect(&format!(
            "Bad binary logic program with (dst_decl, dst_type, operator, dst_ref) = ({dst_decl}, {dst_type}, {operator}, {dst_ref})."
            ));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type != "REAL" && dst_ref == dst_decl
        );
    }

    #[rstest]
    fn test_unary_logic(
        #[values("x")] destination: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] datatype: &str,
        #[values("NEG", "NOT")] operator: &str,
        #[values("x", "not_x")] reference: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {destination} {datatype}
{operator} {reference}
"#
        )).expect(&format!(
            "Bad unary program with (destination, datatype, operator, reference) = ({destination}, {datatype}, {operator}, {reference}"
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            destination == reference
                && ((["REAL", "INTEGER"].contains(&datatype) && operator == "NEG")
                    || (["INTEGER", "BIT", "OCTET"].contains(&datatype) && operator == "NOT"))
        );
    }

    #[rstest]
    fn test_move_memory_references(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("y")] src_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] src_type: &str,
        #[values("x", "not_x")] dst_ref: &str,
        #[values("y", "not_y")] src_ref: &str,
    ) {
        let p = Program::from_str(&format!(
            r#"
DECLARE {dst_decl} {dst_type}
DECLARE {src_decl} {src_type}
MOVE {dst_ref} {src_ref}
"#
        )).expect(&format!(
            "Bad MOVE program with (dst_decl, dst_type, src_decl, src_type, dst_ref, src_ref) = ({dst_decl}, {dst_type}, {src_decl}, {src_type}, {dst_ref}, {src_ref})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            dst_type == src_type && dst_decl == dst_ref && src_decl == src_ref
        );
    }

    #[rstest]
    fn test_move_immediate_values(
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("1", "1.0")] value: &str,
    ) {
        let p = Program::from_str(&format!(
            r#"
DECLARE destination {dst_type}
MOVE destination {value}
"#
        ))
        .expect(&format!(
            "Bad MOVE program with (dst_type, source) = ({dst_type}, {value})."
        ));
        let (f, i) = (f64::from_str(value), i64::from_str(value));
        assert_eq!(
            type_check(&p).is_ok(),
            (dst_type == "REAL" && f.is_ok() && i.is_err()) || (dst_type != "REAL" && i.is_ok())
        );
    }

    #[rstest]
    fn test_exchange(
        #[values("x")] left_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] left_type: &str,
        #[values("y")] right_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] right_type: &str,
        #[values("x", "not_x")] left_ref: &str,
        #[values("y", "not_y")] right_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {left_decl} {left_type}
DECLARE {right_decl} {right_type}
EXCHANGE {left_ref} {right_ref}
"#
        )).expect(&format!(
            "Bad EXCHANGE program with (left_decl, left_type, right_decl, right_type, left_ref, right_ref) = ({left_decl}, {left_type}, {right_decl}, {right_type}, {left_ref}, {right_ref})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            left_decl == left_ref && right_decl == right_ref && left_type == right_type
        );
    }

    #[rstest]
    fn test_load(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("y")] src_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] src_type: &str,
        #[values("z")] offset_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] offset_type: &str,
        #[values("x", "not_z")] dst_ref: &str,
        #[values("y", "not_y")] src_ref: &str,
        #[values("z", "not_z")] offset_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {dst_decl} {dst_type}
DECLARE {src_decl} {src_type}
DECLARE {offset_decl} {offset_type}
LOAD {dst_ref} {src_ref} {offset_ref}
"#
        )).expect(&format!(
            "Bad LOAD program with (dst_decl, dst_type, src_decl, src_type, offset_decl, offset_type, dst_ref, src_ref, offset_ref) = ({dst_decl}, {dst_type}, {src_decl}, {src_type}, {offset_decl}, {offset_type}, {dst_ref}, {src_ref}, {offset_ref})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            (dst_decl == dst_ref && src_decl == src_ref && offset_decl == offset_ref)
                && (dst_type == src_type)
                && (offset_type == "INTEGER")
        );
    }

    #[rstest]
    fn test_store_memory_references(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("y")] src_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] src_type: &str,
        #[values("z")] offset_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] offset_type: &str,
        #[values("x", "not_z")] dst_ref: &str,
        #[values("y", "not_y")] src_ref: &str,
        #[values("z", "not_z")] offset_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {dst_decl} {dst_type}
DECLARE {src_decl} {src_type}
DECLARE {offset_decl} {offset_type}
STORE {dst_ref} {offset_ref} {src_ref}
"#
        )).expect(&format!(
            "Bad STORE program with (dst_decl, dst_type, src_decl, src_type, offset_decl, offset_type, dst_ref, src_ref, offset_ref) = ({dst_decl}, {dst_type}, {src_decl}, {src_type}, {offset_decl}, {offset_type}, {dst_ref}, {src_ref}, {offset_ref})."
        ));
        assert_eq!(
            type_check(&p).is_ok(),
            (dst_decl == dst_ref && src_decl == src_ref && offset_decl == offset_ref)
                && (dst_type == src_type)
                && (offset_type == "INTEGER")
        );
    }

    #[rstest]
    fn test_store_immediate_values(
        #[values("x")] dst_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] dst_type: &str,
        #[values("1.0", "1")] value: &str,
        #[values("z")] offset_decl: &str,
        #[values("REAL", "INTEGER", "BIT", "OCTET")] offset_type: &str,
        #[values("x", "not_z")] dst_ref: &str,
        #[values("z", "not_z")] offset_ref: &str,
    ) {
        let p = Program::from_str(&format!(
                r#"
DECLARE {dst_decl} {dst_type}
DECLARE {offset_decl} {offset_type}
STORE {dst_ref} {offset_ref} {value}
"#
        )).expect(&format!(
            "Bad STORE program with (dst_decl, dst_type, value, offset_decl, offset_type, dst_ref, offset_ref) = ({dst_decl}, {dst_type}, {value}, {offset_decl}, {offset_type}, {dst_ref}, {offset_ref})."
        ));
        let (f, i) = (f64::from_str(value), i64::from_str(value));
        assert_eq!(
            type_check(&p).is_ok(),
            (dst_decl == dst_ref && offset_decl == offset_ref)
                && ((dst_type == "REAL" && f.is_ok() && i.is_err())
                    || (dst_type != "REAL" && i.is_ok()))
                && (offset_type == "INTEGER")
        );
    }
}
