## 0.13.2-rc.0 (2024-10-12)

### Features

#### add source mapping for calibration expansion (#370)

## 0.13.1 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.13.1-rc.0 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.13.0 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### Add waveforms module for generating discrete IQ value sequences representing Quil's defined set of waveforms. (#399)

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

#### support extern call instructions (#394)

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.13.0-rc.2 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### Add waveforms module for generating discrete IQ value sequences representing Quil's defined set of waveforms. (#399)

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

#### support extern call instructions (#394)

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.13.0-rc.1 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### Add waveforms module for generating discrete IQ value sequences representing Quil's defined set of waveforms. (#399)

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

#### support extern call instructions (#394)

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.13.0-rc.0 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### Add waveforms module for generating discrete IQ value sequences representing Quil's defined set of waveforms. (#399)

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.12.2-rc.0 (2024-09-20)

### Features

#### Add waveforms module for generating discrete IQ value sequences representing Quil's defined set of waveforms. (#399)

## 0.12.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.12.1-rc.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.12.1-rc.0

### Fixes

- Update lexical-core

## 0.12.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.12.0-rc.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.11.4

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.11.4-rc.0

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.11.3

### Features

- allow using instruction handler when simplifying program (#395)

## 0.11.3-rc.0

### Features

- allow using instruction handler when simplifying program (#395)

## 0.11.2

### Fixes

- Define correct module path for each instruction type

## 0.11.2-rc.0

### Fixes

- Define correct module path for each instruction type

## 0.11.1

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.11.1-rc.0

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.11.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.11.0-rc.1

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.11.0-rc.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.10.2-rc.0

### Features

- make all unit-only enums Copy (#377)

## 0.10.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.10.1-rc.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.10.1-rc.0

### Features

- add waveform templates (#369)

## 0.10.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.10.0-rc.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.9.1

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.9.1-rc.0

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.9.0

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.9.0-rc.0

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.8.1-rc.0

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.8.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)

## 0.8.0-rc.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)

## 0.7.1

### Features

- linux ppc64le wheel

## 0.7.1-rc.0

### Features

- linux ppc64le wheel

## 0.7.0

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)

## 0.7.0-rc.1

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)

## 0.7.0-rc.0

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

## 0.6.6

### Fixes

- include separators between DEFCIRCUIT parameters (#338)

## 0.6.6-rc.0

### Fixes

- include separators between DEFCIRCUIT parameters (#338)

## 0.6.5

### Fixes

- `is_quil_t()` now correctly returns false for WAIT instructions (#331)

## 0.6.5-rc.0

### Fixes

- `is_quil_t()` now correctly returns false for WAIT instructions (#331)

## 0.6.4

### Fixes

- The `wrap_in_loop` method now applies the end target to the program (#329)

## 0.6.4-rc.0

### Fixes

- The `wrap_in_loop` method now applies the end target to the program (#329)

## 0.6.3

### Features

- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

## 0.6.3-rc.1

### Features

- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

## 0.6.3-rc.0

### Features

- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

## 0.6.2

### Fixes

- use internal QuotedString wrapper to quote Quil strings correctly (#317)

## 0.6.2-rc.0

### Fixes

- use internal QuotedString wrapper to quote Quil strings correctly (#317)

## 0.6.1

### Features

- Support Python 3.12 (#310)

### Fixes

- misc instruction memory accesses (#304)

## 0.6.1-rc.1

### Features

- Support Python 3.12 (#310)

### Fixes

- misc instruction memory accesses (#304)

## 0.6.1-rc.0

### Fixes

- misc instruction memory accesses (#304)

## 0.6.0

### Breaking Changes

- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)

## 0.6.0-rc.0

### Breaking Changes

- Program now has a gate_definitions property that stores all DEFGATEs in a program. These instructions will no longer appear in body_instructions. (#306)

## 0.5.8

### Fixes

- match exactly one qubit for DELAYs without frame specifier (#300)

## 0.5.8-rc.0

### Fixes

- match exactly one qubit for DELAYs without frame specifier (#300)

## 0.5.7

### Fixes

- trigger release

## 0.5.7-rc.0

### Fixes

- trigger release

## 0.5.6

### Fixes

- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs

## 0.5.6-rc.1

### Fixes

- implement PartialOrd correctly for types implementing Ord (#295)
- no percent symbol in variable qubit outputs

## 0.5.6-rc.0

### Fixes

- no percent symbol in variable qubit outputs

## 0.5.5

### Features

- Make in-place addition of Program more efficient (#290)

## 0.5.5-rc.0

### Features

- Make in-place addition of Program more efficient (#290)

## 0.5.4

### Fixes

- Implement not equal comparisons (#289)

## 0.5.4-rc.0

### Fixes

- Implement not equal comparisons (#289)

## 0.5.3

### Fixes

- Allow whitespace to delimit matrix specifications, better support parsing (#286)

## 0.5.3-rc.0

### Fixes

- Allow whitespace to delimit matrix specifications, better support parsing (#286)

## 0.5.2

### Features

- Add get_qubits method to Instruction
- Instruction classes now implement `__copy__`, and `__deepcopy__`, making them compatible with Python's `copy` module. (#283)

## 0.5.2-rc.1

### Features

- Add get_qubits method to Instruction
- Instruction classes now implement `__copy__`, and `__deepcopy__`, making them compatible with Python's `copy` module. (#283)

## 0.5.2-rc.0

### Features

- Instruction classes now implement `__copy__`, and `__deepcopy__`, making them compatible with Python's `copy` module. (#283)

## 0.5.1

### Features

- Build & publish wheels for Windows (#280)

## 0.5.1-rc.1

### Features

- Build & publish wheels for Windows (#280)

## 0.5.1-rc.0

### Features

- Build & publish wheels for Windows (#280)

## 0.5.0

### Breaking Changes

- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)

## 0.5.0-rc.1

### Breaking Changes

- Support for Qubit and Target Placeholdres have been added. Converting programs and instructions to a string has been removed and replaced with a fallible to_quil() method. The `Label` struct has been repurposed to support `Label` instructions specifically. The `Target` enum has been added to express `@targets` as part of an instruction. (#266)
- Decouple expression hashing and equality (#277)

## 0.5.0-rc.0

### Breaking Changes

- Decouple expression hashing and equality (#277)

## 0.4.0

### Breaking Changes

- allow overriding Instruction getters (#260)

### Features

- Add setters for `Program` calibrations, waveforms, frames, and memory_regions (#264)

## 0.4.0-rc.2

### Breaking Changes

- allow overriding Instruction getters (#260)

### Features

- Add setters for `Program` calibrations, waveforms, frames, and memory_regions (#264)

## 0.4.0-rc.1

### Breaking Changes

- allow overriding Instruction getters (#260)

### Features

- Add setters for `Program` calibrations, waveforms, frames, and memory_regions (#264)

## 0.4.0-rc.0

### Breaking Changes

- allow overriding Instruction getters (#260)

## 0.3.0

### Breaking Changes

- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)

## 0.3.0-rc.0

### Breaking Changes

- This release is identical to 0.18.0. An error in our CI caused a continuity error with our published releases. (#254)

## 0.2.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.3.0-rc.1

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.3.0-rc.0

### Breaking Changes

- When adding two Programs, the resulting Program will have a correct used qubit cache. (#249)
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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0

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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0-rc.14

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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0-rc.13

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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0-rc.12

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

- Correct precedence rules are followed when matching measure calibrations. (#243)
- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0-rc.11

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

- Expand all analog control instructions (#238)
- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.2.0-rc.10

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

## 0.2.0-rc.9

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

## 0.2.0-rc.8

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

## 0.2.0-rc.7

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

## 0.2.0-rc.6

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

## 0.2.0-rc.5

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

## 0.2.0-rc.4

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

## 0.2.0-rc.3

### Breaking Changes

- optimize clones and collections (#228)
- Expression parameter strings are now delimited by a comma. `get_expression_parameter_string` and `get_string_parameter_string` have been removed from the instruction module. (#214)

### Features

- Most instruction types are now hashable.

### Fixes

- documentation typo
- The destination and source are no longer flipped when parsing CONVERT instructions. (#226)
- BinaryLogic now has it's own to string implementation (#222)

## 0.1.0

This first release of the `quil` package exposes a Python API for most of the `quil-rs` package. This includes a parser, as well as types for instructions, programs, and expressions.
