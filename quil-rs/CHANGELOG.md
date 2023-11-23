## 0.22.2

### Features

- impl FromStr for FrameIdentifier (#312)

### Fixes

- use internal QuotedString wrapper to quote Quil strings correctly (#317)

## 0.22.2-rc.1

### Features

- impl FromStr for FrameIdentifier (#312)

### Fixes

- use internal QuotedString wrapper to quote Quil strings correctly (#317)

## 0.22.2-rc.0

### Features

- impl FromStr for FrameIdentifier (#312)

## 0.22.1

### Fixes

- misc instruction memory accesses (#304)

## 0.22.1-rc.1

### Fixes

- misc instruction memory accesses (#304)

## 0.22.1-rc.0

### Fixes

- misc instruction memory accesses (#304)

## 0.22.0

### Breaking Changes

- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)

## 0.22.0-rc.0

### Breaking Changes

- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)

## 0.21.7

### Fixes

- match exactly one qubit for DELAYs without frame specifier (#300)

## 0.21.7-rc.0

### Fixes

- match exactly one qubit for DELAYs without frame specifier (#300)

## 0.21.6

### Fixes

- trigger release

## 0.21.6-rc.0

### Fixes

- trigger release

## 0.21.5

### Fixes

- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs

## 0.21.5-rc.1

### Fixes

- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs

## 0.21.5-rc.0

### Fixes

- no percent symbol in variable qubit outputs

## 0.21.4

### Features

- Make in-place addition of Program more efficient (#290)

## 0.21.4-rc.0

### Features

- Make in-place addition of Program more efficient (#290)

## 0.21.3

### Fixes

- Implement not equal comparisons (#289)

## 0.21.3-rc.0

### Fixes

- Implement not equal comparisons (#289)

## 0.21.2

### Fixes

- Allow whitespace to delimit matrix specifications, better support parsing (#286)

## 0.21.2-rc.0

### Fixes

- Allow whitespace to delimit matrix specifications, better support parsing (#286)

## 0.21.1

### Features

- Add get_qubits method to Instruction

## 0.21.1-rc.0

### Features

- Add get_qubits method to Instruction

## 0.21.0

### Breaking Changes

- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)

### Features

- MemoryReference implements Ord (#275)

## 0.21.0-rc.1

### Breaking Changes

- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)

### Features

- MemoryReference implements Ord (#275)

## 0.21.0-rc.0

### Breaking Changes

- Decouple expression hashing and equality (#277)

### Features

- MemoryReference implements Ord (#275)

## 0.20.1-rc.1

### Features

- MemoryReference implements Ord (#275)

## 0.20.1-rc.0

### Features

- MemoryReference implements Ord (#275)

## 0.20.0

### Breaking Changes

- allow overriding Instruction getters (#260)

### Fixes

- infix expression parenthesization (#262)
- escape Quil strings for display (#258)

## 0.20.0-rc.2

### Breaking Changes

- allow overriding Instruction getters (#260)

### Fixes

- infix expression parenthesization (#262)
- escape Quil strings for display (#258)

## 0.20.0-rc.1

### Breaking Changes

- allow overriding Instruction getters (#260)

### Fixes

- infix expression parenthesization (#262)
- escape Quil strings for display (#258)

## 0.20.0-rc.0

### Breaking Changes

- allow overriding Instruction getters (#260)

### Fixes

- infix expression parenthesization (#262)
- escape Quil strings for display (#258)

## 0.19.1-rc.1

### Fixes

- infix expression parenthesization (#262)
- escape Quil strings for display (#258)

## 0.19.1-rc.0

### Fixes

- escape Quil strings for display (#258)

## 0.19.0

### Breaking Changes

- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)

## 0.19.0-rc.0

### Breaking Changes

- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)

## 0.18.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.19.0-rc.1

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.19.0-rc.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.14

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- wasm-bindgen cargo feature
- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.13

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- associate & commute multiplication & addition in expression simplification (#245)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.12

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.11

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- Expand all analog control instructions (#238)
- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.10

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- 'simplify' pi expression to floating-point form (#240)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.9

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Program::into_instructions (#242)
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.8

### Breaking Changes

- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.7

### Breaking Changes

- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.6

### Breaking Changes

- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.5

### Breaking Changes

- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- build and return the unitary of a program (#213)
- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.4

### Breaking Changes

- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.18.0-rc.3

### Breaking Changes

- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.17.0

### Breaking Changes

- The `Infix`, `Prefix`, and `FunctionCall` variants of `Expression` are no longer struct variants. They now are tuple variants with an inner `InfixExpression`, `PrefixExpression`, and `FunctionCallExpression`, respectively.
- The `GateType` and `GateSpecification` enums now include a `PauliSum` variant.
- `program::error::ProgramError<T>` has been renamed to `ParseProgramError<T>`
- `Program` methods that return a result now use `program::ProgramError` as the error type.
- `Program.to_headers()` has been removed. This has affected two other methods:
    - `Program.to_string()` is now implemented via `std::fmt::Display` and doesn't take an argument to include "headers", they are always included in the string.
    - `Program.to_instructions()` no longer takes an argument to include headers, what were called "headers" are always included.
- `CalibrationSet::get_match_for_gate` now takes a single `Gate` as a parameter.

### Features

- `PauliSum` `GateSpecification`s are now supported.
- All instruction types now have a `new` constructor, which perform extra validation, if appropriate.
- The `Gate` type now has methods for applying `DAGGER`, `CONTROLLED`, and `FORKED` modifiers.
- `SWAP-PHASES` is now supported.
- `WAIT` is now supported.
- `DECLARE` with offset and sharing is now supported.
- `Program`s can now be appended with the `+` operator.
- `get` and `insert` methods have been added to `FrameSet`
- `CalibrationSet` now has getter methods for `calibrations` and `measure_calibrations`.
- `CalibrationSet` now has a `get_match_for_measurement` method.
- `Program` now has a `dagger` method for applying `DAGGER` modifiers to all gates in a program.

### Fixes

- Casing is now ignored when parsing function call expressions.
- Parameters now include the leading `%` when printed with `std::fmt::Display`.
- `DEFCAL MEASURE` can now be parsed with a variable qubit and an address, instead of just one or the other.


## 0.16.0

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- The parser now follows the correct precedence rules for ungrouped infix expressions (#207)
- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.6

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- The parser now follows the correct precedence rules for ungrouped infix expressions (#207)
- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.5

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.4

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.3

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- parse SWAP-PHASES (#200)
- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.2

### Breaking Changes

- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version

### Fixes

- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.16.0-rc.1

### Breaking Changes

- empty commit so knope calculates current version

### Fixes

- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)

## 0.10.0-rc.2

### Breaking Changes

- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)
- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.1

### Breaking Changes

- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)
- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.0

### Breaking Changes

- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- allow for variable qubit in DEFCAL MEASURE
- correctly expand delays (#142)
- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.16.0-rc.0

### Breaking Changes

- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- correctly expand delays (#142)
- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.15.0

### Breaking Changes

- empty commit to force version bump

## 0.10.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.2

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- update snapshots
- Waveforms w/o params need no parens
- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.1

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- support escaped double quotes and backslashes in strings (#120)
- make dynamic error Sync as well (#131)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Support Expression arithmetic operations (#126)
- Program.into_simplified
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.15.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.14.2

### Features

- Support Expression arithmetic operations (#126)

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)

## 0.14.2-rc.0

### Features

- Support Expression arithmetic operations (#126)

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)

## 0.10.0-rc.1

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.1

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- make dynamic error Sync as well (#131)
- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.11.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference

### Fixes

- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.10.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- impl FromStr for MemoryReference

### Fixes

- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)

## 0.15.0-rc.0

### Breaking Changes

- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- impl FromStr for MemoryReference

### Fixes

- support escaped double quotes and backslashes in strings (#120)
- fix performance regression (#113)
- do not get line/column info for tokens except on error
- require dynamic error to by Send (#108)
- bump thiserror version and update import name (#103)
- identifier parser (#100)
- test cases with rstest
- test cases should not violate the spec
- remove a `dbg!` statement left over from #88
- use structured error
- update node version and dependencies for semantic-release (#84)
- update semantic-release version as per dependabot suggestion (#83)
- DEFCAL MEASURE serialization
- test roundtrip of program->string->program
- linting
- Instruction used/blocked frames calculation (#74)
