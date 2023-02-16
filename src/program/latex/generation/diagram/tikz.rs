use std::fmt::format;

pub mod quantikz;

pub trait Tikz {
    fn generate_default_quantikz_diagram(&self);
}

pub enum TikzOperator {
    TikzLeftKet(u32),
    TikzControl(i32),
    TikzCnotTarget,
    TikzCphaseTarget,
    TikzSwap(i32),
    TikzSwapTarget,
    TikzNop,
    TikzMeasure,
}

impl TikzOperator {
    fn get_tikz_operator(tikz_operator: Self) -> String {
        match tikz_operator {
            Self::TikzLeftKet(qubit) => format(format_args!(r#"\lstick{{\ket{{q_{{{qubit}}}}}}}"#)), // \lstick{\ket{q_{qubit}}}
            Self::TikzControl(offset) => format(format_args!(r#"\ctrl{{{offset}}}"#)), // \ctrl{offset}
            Self::TikzCnotTarget => r"\targ{}".to_string(), // \targ{}
            Self::TikzCphaseTarget => r"\control{}".to_string(), // \control{}
            Self::TikzSwap(offset) => format(format_args!(r"\swap{{{offset}}}")), // \swap{offset}
            Self::TikzSwapTarget => r"\targX{}".to_string(), // \targX{}
            Self::TikzNop => r"\qw".to_string(), // \qw
            Self::TikzMeasure => r"\meter{}".to_string(), // \meter{}
        }
    }
}

#[cfg(test)]
mod tests {
    mod tikz_operators {
        use super::super::TikzOperator;

        #[test]
        fn test_tikz_left_ket() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzLeftKet(0)));
        }

        #[test]
        fn test_tikz_control() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzControl(2)));
        }

        #[test]
        fn test_tikz_cnot_target() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzCnotTarget));
        }

        #[test]
        fn test_tikz_cphase_target() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzCphaseTarget));
        }

        #[test]
        fn test_tikz_swap() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzSwap(4)));
        }

        #[test]
        fn test_tikz_swap_target() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzSwapTarget));
        }

        #[test]
        fn test_tikz_nop() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzNop));
        }

        #[test]
        fn test_tikz_measure() {
            insta::assert_snapshot!(TikzOperator::get_tikz_operator(TikzOperator::TikzMeasure));
        }
    }
}