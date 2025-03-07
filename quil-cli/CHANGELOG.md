## 0.8.0-rc.0 (2025-03-07)

### Breaking Changes

#### intern `Expression`s

#### expression interning, including caching during simplification

### Features

#### implement arithmetic traits for `ArcIntern<Expression>`

#### expression simplification takes advantage of sharing

#### add missing `Neg` impl for `Expression`

#### cache expression sizes during simplification

### Fixes

#### fix simplification so that a/(bc) = (a/b)/c

#### fix simplification so that a/(bc) = (a/b)/c (#442)

## 0.7.2-rc.0 (2025-03-05)

### Fixes

#### fix simplification so that a/(bc) = (a/b)/c

#### fix simplification so that a/(bc) = (a/b)/c (#442)

## 0.7.1 (2025-02-18)

### Fixes

#### prefer data transparency across ffi

## 0.7.1-rc.0 (2025-02-18)

### Fixes

#### prefer data transparency across ffi

## 0.7.0 (2025-01-17)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### make NaN == NaN inside Expression

#### tokenize keywords and identifiers correctly (#428)

#### track memory depedencies involving the block terminator

#### make NaN == NaN inside Expression (#318)

#### track memory dependencies involving the block terminator (#433)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.7.0-rc.2 (2025-01-17)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### make NaN == NaN inside Expression

#### tokenize keywords and identifiers correctly (#428)

#### track memory depedencies involving the block terminator

#### make NaN == NaN inside Expression (#318)

#### track memory dependencies involving the block terminator (#433)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.7.0-rc.1 (2025-01-14)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### make NaN == NaN inside Expression

#### tokenize keywords and identifiers correctly (#428)

#### make NaN == NaN inside Expression (#318)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.7.0-rc.0 (2025-01-10)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### tokenize keywords and identifiers correctly (#428)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.6.3-rc.0 (2025-01-09)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.6.2 (2024-11-06)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

### Fixes

#### box nested ExternError variants (#419)

## 0.6.2-rc.2 (2024-11-06)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

### Fixes

#### box nested ExternError variants (#419)

## 0.6.2-rc.1 (2024-11-01)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

## 0.6.2-rc.0 (2024-10-12)

### Features

#### add source mapping for calibration expansion (#370)

## 0.6.1 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.6.1-rc.0 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.6.0 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.6.0-rc.2 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.6.0-rc.1 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.6.0-rc.0 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.5.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.5.1-rc.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.5.1-rc.0

### Fixes

- Update lexical-core

## 0.5.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.5.0-rc.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.4.3

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.4.3-rc.0

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.4.2

### Features

- allow using instruction handler when simplifying program (#395)

## 0.4.2-rc.0

### Features

- allow using instruction handler when simplifying program (#395)

## 0.4.1

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.4.1-rc.0

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.4.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.4.0-rc.1

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.4.0-rc.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.3.2-rc.0

### Features

- make all unit-only enums Copy (#377)

## 0.3.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.3.1-rc.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.3.1-rc.0

### Features

- add waveform templates (#369)

## 0.3.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.3.0-rc.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.2.1

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.2.1-rc.0

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.2.0

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.2.0-rc.1

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.1.1-rc.0

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

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
