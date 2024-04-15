## 0.1.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)
- #334: program scheduling and analysis utilities (#336)
- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)
- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)
- allow overriding Instruction getters (#260)
- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)
- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)
- Release quil-py
- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version
- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Add CLI for interacting with quil-rs (#348)
- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs
- Make in-place addition of Program more efficient (#290)
- Add get_qubits method to Instruction
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.
- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference
- update program to use btreemap for deterministic ordering

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)
- include separators between DEFCIRCUIT parameters (#338)
- `is_quil_t()` now correctly returns false for WAIT instructions (#331)
- The `wrap_in_loop` method now applies the end target to the program (#329)
- use internal QuotedString wrapper to quote Quil strings correctly (#317)
- misc instruction memory accesses (#304)
- match exactly one qubit for DELAYs without frame specifier (#300)
- trigger release
- calibration definitions don't contribute to Instrution::get_qubits
- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs
- Implement not equal comparisons (#289)
- Allow whitespace to delimit matrix specifications, better support parsing (#286)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)
- The parser now follows the correct precedence rules for ungrouped infix expressions (#207)
- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
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
- cargo fmt & passing tests
- PRAGMA instruction `Display` impl (#62)
- change quilc submodule to use HTTPS
- move cargo update inline with semantic-release prepare cmd
- lex keywords from identifiers (#47)
- DEFWAVEFORM and waveform name parsing (#40)
- remove unused imports
- `Program::from_str` and `Expression::from_str` will no longer panic on bad input. (#37)
- Allow indented comments when parsing. (#24)
- Unbox calibrations and waveform invocations
- parse and print real values as double precision
- support Windows line separators \r\n
- make program::graph public (#4)

## 0.1.0-rc.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)
- #334: program scheduling and analysis utilities (#336)
- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)
- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)
- allow overriding Instruction getters (#260)
- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)
- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)
- Release quil-py
- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version
- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Add CLI for interacting with quil-rs (#348)
- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs
- Make in-place addition of Program more efficient (#290)
- Add get_qubits method to Instruction
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.
- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference
- update program to use btreemap for deterministic ordering

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)
- include separators between DEFCIRCUIT parameters (#338)
- `is_quil_t()` now correctly returns false for WAIT instructions (#331)
- The `wrap_in_loop` method now applies the end target to the program (#329)
- use internal QuotedString wrapper to quote Quil strings correctly (#317)
- misc instruction memory accesses (#304)
- match exactly one qubit for DELAYs without frame specifier (#300)
- trigger release
- calibration definitions don't contribute to Instrution::get_qubits
- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs
- Implement not equal comparisons (#289)
- Allow whitespace to delimit matrix specifications, better support parsing (#286)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)
- The parser now follows the correct precedence rules for ungrouped infix expressions (#207)
- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
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
- cargo fmt & passing tests
- PRAGMA instruction `Display` impl (#62)
- change quilc submodule to use HTTPS
- move cargo update inline with semantic-release prepare cmd
- lex keywords from identifiers (#47)
- DEFWAVEFORM and waveform name parsing (#40)
- remove unused imports
- `Program::from_str` and `Expression::from_str` will no longer panic on bad input. (#37)
- Allow indented comments when parsing. (#24)
- Unbox calibrations and waveform invocations
- parse and print real values as double precision
- support Windows line separators \r\n
- make program::graph public (#4)

## 0.2.0-rc.0

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)
- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)
- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)
- allow overriding Instruction getters (#260)
- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)
- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
- cache used qubits on Program (#234)
- Program::get_frames_for_instruction return type
- more optimizations (#233)
- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)
- Release quil-py
- introduce ExecutionDependency::Scheduled (#186)
- empty commit so knope calculates current version
- empty commit to force version bump
- genericize parsing errors and remove error Strings
- fix all compilation errors from error refactor

### Features

- Add CLI for interacting with quil-rs (#348)
- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs
- Make in-place addition of Program more efficient (#290)
- Add get_qubits method to Instruction
- add clone_without_body_instructions to Program (#236)
- build and return the unitary of a program (#213)
- Most instruction types are now hashable.
- RESET frame computation
- Program.into_simplified
- Support Expression arithmetic operations (#126)
- support INCLUDE
- support CONVERT
- support NOP
- impl FromStr for MemoryReference
- update program to use btreemap for deterministic ordering

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)
- include separators between DEFCIRCUIT parameters (#338)
- `is_quil_t()` now correctly returns false for WAIT instructions (#331)
- The `wrap_in_loop` method now applies the end target to the program (#329)
- use internal QuotedString wrapper to quote Quil strings correctly (#317)
- misc instruction memory accesses (#304)
- match exactly one qubit for DELAYs without frame specifier (#300)
- trigger release
- calibration definitions don't contribute to Instrution::get_qubits
- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs
- Implement not equal comparisons (#289)
- Allow whitespace to delimit matrix specifications, better support parsing (#286)
- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)
- The parser now follows the correct precedence rules for ungrouped infix expressions (#207)
- The imaginary part of a complex number will now always be formatted as a floating point number. (#205)
- mark additional instructions as scheduled (#203)
- parse SWAP-PHASES (#200)
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
- cargo fmt & passing tests
- PRAGMA instruction `Display` impl (#62)
- change quilc submodule to use HTTPS
- move cargo update inline with semantic-release prepare cmd
- lex keywords from identifiers (#47)
- DEFWAVEFORM and waveform name parsing (#40)
- remove unused imports
- `Program::from_str` and `Expression::from_str` will no longer panic on bad input. (#37)
- Allow indented comments when parsing. (#24)
- Unbox calibrations and waveform invocations
- parse and print real values as double precision
- support Windows line separators \r\n
- make program::graph public (#4)
