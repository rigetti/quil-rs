## 0.34.0 (2026-01-14)

### Breaking Changes

- defgate as sequence
- change `InstructionHandler` to be a trait and require it everywhere in Rust (#484)

## 0.34.0-rc.3 (2026-01-13)

### Breaking Changes

- defgate as sequence
- change `InstructionHandler` to be a trait and require it everywhere in Rust (#484)

## 0.34.0-rc.2 (2025-11-07)

### Breaking Changes

- defgate as sequence

## 0.34.0-rc.1 (2025-11-06)

### Breaking Changes

- defgate as sequence

## 0.34.0-rc.0 (2025-11-05)

### Breaking Changes

- defgate as sequence

## 0.33.0 (2025-10-17)

### Breaking Changes

- merge quil-py into quil-rs (#462)
- memory dependencies correctly remember writes (#477)
- add named measurements (#479)
- add support for more numeric literals, as per quil-lang/quil#93 (#483)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)
- per @mingyoungjeng, try unifying `MemoryAccessQueue` and `PreviousNodes`

### Fixes

- memory access dependency computation correctly remembers writes

## 0.33.0-rc.10 (2025-10-17)

### Breaking Changes

- merge quil-py into quil-rs (#462)
- memory dependencies correctly remember writes (#477)
- add named measurements (#479)
- add support for more numeric literals, as per quil-lang/quil#93 (#483)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)
- per @mingyoungjeng, try unifying `MemoryAccessQueue` and `PreviousNodes`

### Fixes

- memory access dependency computation correctly remembers writes

## 0.33.0-rc.9 (2025-10-15)

### Breaking Changes

- merge quil-py into quil-rs (#462)
- memory dependencies correctly remember writes (#477)
- add named measurements (#479)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)
- per @mingyoungjeng, try unifying `MemoryAccessQueue` and `PreviousNodes`

### Fixes

- memory access dependency computation correctly remembers writes

## 0.33.0-rc.8 (2025-10-10)

### Breaking Changes

- merge quil-py into quil-rs (#462)
- memory dependencies correctly remember writes (#477)
- add named measurements (#479)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)
- per @mingyoungjeng, try unifying `MemoryAccessQueue` and `PreviousNodes`

### Fixes

- memory access dependency computation correctly remembers writes

## 0.33.0-rc.7 (2025-10-09)

### Breaking Changes

- merge quil-py into quil-rs (#462)
- memory dependencies correctly remember writes (#477)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)
- per @mingyoungjeng, try unifying `MemoryAccessQueue` and `PreviousNodes`

### Fixes

- memory access dependency computation correctly remembers writes

## 0.33.0-rc.6 (2025-10-09)

### Breaking Changes

- merge quil-py into quil-rs (#462)

### Features

- harmonize `PartialEq`, `Eq`, `Hash`, and floating point numbers (#474)

## 0.33.0-rc.5 (2025-10-08)

### Breaking Changes

- merge quil-py into quil-rs (#462)

## 0.33.0-rc.4 (2025-10-06)

### Breaking Changes

- Drop support for Python 3.9.
- fix getters for Call arguments
- remove unused-but-public `MemoryAccess` type
- rename `Calibration` to `CalibrationDefinition` and clean up the associated code
- fix `MeasureCalibrationDefinition`'s fields
- backport breaking changes from #479 (#482)
- upgrade pyo3 and stub_gen versions
- use SCREAMING_SNAKE_CASE for simple enums

### Features

- add pickle impls for instructions
- unite Instruction macros
- impl __getnewargs__
- feature-gate python-related elements
- make WaveformInvocation pickleable
- allow stripping only stub_gen attrs
- generate python stubs
- improve linting script
- add command to generate stub files
- add more generated stubs
- add more stub gen attributes
- generate stubs for base Quil exception
- improve lint script stub attr discovery
- make enums copyable and pickleable
- teach lint script a bunch more tricks
- strip more pyo3 attrs
- update Python support
- let knope manage quil-cli version
- catch suspicious method names
- generate `.pyi` stub files' contents in a consistent order
- generate `.pyi` stub files' contents in a consistent order (#481)
- correctly set module for error stubs
- add __getnewargs__ to BinaryOperand
- set default log-level to warning
- teach linter to catch missing rename_all
- upgrade pyo3-stub-gen and remove workarounds

### Fixes

- add missing parse impls
- add missing rename annotation
- expose missing error types
- PiConstant should be named pi in Python
- Program.filter_instruction from Python should return a Program
- CalibrationIdentifier uses different argument order for Python constructor
- add missing Python constructor for WaveformInvocation
- Python version of TimeSpanSeconds calls "start_time" just "start"
- adjust tests to fix modified quil API
- remove unneeded get_all annotation on enum
- typos and mistakes in module exports
- transfer subclass annotations
- gate enum naming
- add getters for Gate
- getters for interned expressions
- enum casing
- add missing constructors
- add missing dagger method
- share to_real impl
- allow mutating FrameDefinition
- FunctionCallExpression.expression is a getter
- make parse static, not classmethod
- allow setting properties on non-hash classes
- restore module def on Expression
- add missing module metadata
- badly replaced error names
- another badly replaced error name
- add InstructionError to module
- update validation module properties
- make GateDefinition pickleable
- make FrameIdentifier pickleable
- significant clean-up for pickling
- correct broken tests
- to_unitary name and bounds
- update sync_versions script
- use correct macro for QuilError
- use an f-string syntax supported in older pythons
- restore original waveform public items
- output correct types for some stub overrides

## 0.33.0-rc.3 (2025-09-30)

### Breaking Changes

- Drop support for Python 3.9.
- fix getters for Call arguments
- remove unused-but-public `MemoryAccess` type
- rename `Calibration` to `CalibrationDefinition` and clean up the associated code
- fix `MeasureCalibrationDefinition`'s fields
- backport breaking changes from #479 (#482)

### Features

- add pickle impls for instructions
- unite Instruction macros
- impl __getnewargs__
- feature-gate python-related elements
- make WaveformInvocation pickleable
- allow stripping only stub_gen attrs
- generate python stubs
- improve linting script
- add command to generate stub files
- add more generated stubs
- add more stub gen attributes
- generate stubs for base Quil exception
- improve lint script stub attr discovery
- make enums copyable and pickleable
- teach lint script a bunch more tricks
- strip more pyo3 attrs
- update Python support
- let knope manage quil-cli version
- catch suspicious method names
- generate `.pyi` stub files' contents in a consistent order
- generate `.pyi` stub files' contents in a consistent order (#481)

### Fixes

- add missing parse impls
- add missing rename annotation
- expose missing error types
- PiConstant should be named pi in Python
- Program.filter_instruction from Python should return a Program
- CalibrationIdentifier uses different argument order for Python constructor
- add missing Python constructor for WaveformInvocation
- Python version of TimeSpanSeconds calls "start_time" just "start"
- adjust tests to fix modified quil API
- remove unneeded get_all annotation on enum
- typos and mistakes in module exports
- transfer subclass annotations
- gate enum naming
- add getters for Gate
- getters for interned expressions
- enum casing
- add missing constructors
- add missing dagger method
- share to_real impl
- allow mutating FrameDefinition
- FunctionCallExpression.expression is a getter
- make parse static, not classmethod
- allow setting properties on non-hash classes
- restore module def on Expression
- add missing module metadata
- badly replaced error names
- another badly replaced error name
- add InstructionError to module
- update validation module properties
- make GateDefinition pickleable
- make FrameIdentifier pickleable
- significant clean-up for pickling
- correct broken tests
- to_unitary name and bounds
- update sync_versions script
- use correct macro for QuilError
- use an f-string syntax supported in older pythons
- restore original waveform public items
- output correct types for some stub overrides

## 0.33.0-rc.2 (2025-09-17)

### Breaking Changes

- Drop support for Python 3.9.

### Features

- add pickle impls for instructions
- unite Instruction macros
- impl __getnewargs__
- feature-gate python-related elements
- make WaveformInvocation pickleable
- allow stripping only stub_gen attrs
- generate python stubs
- improve linting script
- add command to generate stub files
- add more generated stubs
- add more stub gen attributes
- generate stubs for base Quil exception
- improve lint script stub attr discovery
- make enums copyable and pickleable
- teach lint script a bunch more tricks
- strip more pyo3 attrs
- update Python support
- let knope manage quil-cli version

### Fixes

- add missing parse impls
- add missing rename annotation
- expose missing error types
- PiConstant should be named pi in Python
- Program.filter_instruction from Python should return a Program
- CalibrationIdentifier uses different argument order for Python constructor
- add missing Python constructor for WaveformInvocation
- Python version of TimeSpanSeconds calls "start_time" just "start"
- adjust tests to fix modified quil API
- remove unneeded get_all annotation on enum
- typos and mistakes in module exports
- transfer subclass annotations
- gate enum naming
- add getters for Gate
- getters for interned expressions
- enum casing
- add missing constructors
- add missing dagger method
- share to_real impl
- allow mutating FrameDefinition
- FunctionCallExpression.expression is a getter
- make parse static, not classmethod
- allow setting properties on non-hash classes
- restore module def on Expression
- add missing module metadata
- badly replaced error names
- another badly replaced error name
- add InstructionError to module
- update validation module properties
- make GateDefinition pickleable
- make FrameIdentifier pickleable
- significant clean-up for pickling
- correct broken tests
- to_unitary name and bounds
- update sync_versions script
- use correct macro for QuilError
- use an f-string syntax supported in older pythons
- restore original waveform public items

## 0.33.0-rc.1 (2025-09-17)

### Breaking Changes

- Drop support for Python 3.9.

### Features

- add pickle impls for instructions
- unite Instruction macros
- impl __getnewargs__
- feature-gate python-related elements
- make WaveformInvocation pickleable
- allow stripping only stub_gen attrs
- generate python stubs
- improve linting script
- add command to generate stub files
- add more generated stubs
- add more stub gen attributes
- generate stubs for base Quil exception
- improve lint script stub attr discovery
- make enums copyable and pickleable
- teach lint script a bunch more tricks
- strip more pyo3 attrs
- update Python support
- let knope manage quil-cli version

### Fixes

- add missing parse impls
- add missing rename annotation
- expose missing error types
- PiConstant should be named pi in Python
- Program.filter_instruction from Python should return a Program
- CalibrationIdentifier uses different argument order for Python constructor
- add missing Python constructor for WaveformInvocation
- Python version of TimeSpanSeconds calls "start_time" just "start"
- adjust tests to fix modified quil API
- remove unneeded get_all annotation on enum
- typos and mistakes in module exports
- transfer subclass annotations
- gate enum naming
- add getters for Gate
- getters for interned expressions
- enum casing
- add missing constructors
- add missing dagger method
- share to_real impl
- allow mutating FrameDefinition
- FunctionCallExpression.expression is a getter
- make parse static, not classmethod
- allow setting properties on non-hash classes
- restore module def on Expression
- add missing module metadata
- badly replaced error names
- another badly replaced error name
- add InstructionError to module
- update validation module properties
- make GateDefinition pickleable
- make FrameIdentifier pickleable
- significant clean-up for pickling
- correct broken tests
- to_unitary name and bounds
- update sync_versions script
- use correct macro for QuilError
- use an f-string syntax supported in older pythons
- restore original waveform public items

## 0.33.0-rc.0 (2025-09-17)

### Breaking Changes

- Drop support for Python 3.9.

### Features

- add pickle impls for instructions
- unite Instruction macros
- impl __getnewargs__
- feature-gate python-related elements
- make WaveformInvocation pickleable
- allow stripping only stub_gen attrs
- generate python stubs
- improve linting script
- add command to generate stub files
- add more generated stubs
- add more stub gen attributes
- generate stubs for base Quil exception
- improve lint script stub attr discovery
- make enums copyable and pickleable
- teach lint script a bunch more tricks
- strip more pyo3 attrs
- update Python support
- let knope manage quil-cli version

### Fixes

- add missing parse impls
- add missing rename annotation
- expose missing error types
- PiConstant should be named pi in Python
- Program.filter_instruction from Python should return a Program
- CalibrationIdentifier uses different argument order for Python constructor
- add missing Python constructor for WaveformInvocation
- Python version of TimeSpanSeconds calls "start_time" just "start"
- adjust tests to fix modified quil API
- remove unneeded get_all annotation on enum
- typos and mistakes in module exports
- transfer subclass annotations
- gate enum naming
- add getters for Gate
- getters for interned expressions
- enum casing
- add missing constructors
- add missing dagger method
- share to_real impl
- allow mutating FrameDefinition
- FunctionCallExpression.expression is a getter
- make parse static, not classmethod
- allow setting properties on non-hash classes
- restore module def on Expression
- add missing module metadata
- badly replaced error names
- another badly replaced error name
- add InstructionError to module
- update validation module properties
- make GateDefinition pickleable
- make FrameIdentifier pickleable
- significant clean-up for pickling
- correct broken tests
- to_unitary name and bounds
- update sync_versions script
- use correct macro for QuilError
- use an f-string syntax supported in older pythons
- restore original waveform public items

## 0.32.0 (2025-08-27)

### Breaking Changes

- set Python and dependency versions to match
- set Python and dependency versions to match
- remove unnecessary getter methods on `MatchedFrames`
- improvements to `MatchedFrames` and related methods (#333)

### Features

- add waveform constructors
- derive Default for MatchedFrames
- add `MatchedFrames::new`
- improve lifetimes of `InstructionHandler::matching_frames`

### Fixes

- waveform constructor parameter names
- improve lifetime – thanks, Clippy!
- lifetimes in Program#get_frames_for_instruction

## 0.32.0-rc.4 (2025-08-27)

### Breaking Changes

- set Python and dependency versions to match
- set Python and dependency versions to match
- remove unnecessary getter methods on `MatchedFrames`
- improvements to `MatchedFrames` and related methods (#333)

### Features

- add waveform constructors
- derive Default for MatchedFrames
- add `MatchedFrames::new`
- improve lifetimes of `InstructionHandler::matching_frames`

### Fixes

- waveform constructor parameter names
- improve lifetime – thanks, Clippy!
- lifetimes in Program#get_frames_for_instruction

## 0.32.0-rc.3 (2025-08-27)

### Breaking Changes

- set Python and dependency versions to match
- set Python and dependency versions to match

### Features

- add waveform constructors

### Fixes

- waveform constructor parameter names
- improve lifetime – thanks, Clippy!

## 0.32.0-rc.2 (2025-07-02)

### Breaking Changes

#### set Python and dependency versions to match

#### set Python and dependency versions to match

### Features

#### add waveform constructors

### Fixes

#### waveform constructor parameter names

## 0.32.0-rc.1 (2025-07-01)

### Breaking Changes

#### set Python and dependency versions to match

#### set Python and dependency versions to match

### Features

#### add waveform constructors

### Fixes

#### waveform constructor parameter names

## 0.32.0-rc.0 (2025-06-30)

### Breaking Changes

#### set Python and dependency versions to match

#### set Python and dependency versions to match

## 0.31.0 (2025-03-07)

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

## 0.31.0-rc.0 (2025-03-07)

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

## 0.30.2-rc.0 (2025-03-05)

### Fixes

#### fix simplification so that a/(bc) = (a/b)/c

#### fix simplification so that a/(bc) = (a/b)/c (#442)

## 0.30.1 (2025-02-18)

### Fixes

#### prefer data transparency across ffi

## 0.30.1-rc.0 (2025-02-18)

### Fixes

#### prefer data transparency across ffi

## 0.30.0 (2025-01-17)

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

## 0.30.0-rc.2 (2025-01-17)

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

## 0.30.0-rc.1 (2025-01-14)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### make NaN == NaN inside Expression

#### tokenize keywords and identifiers correctly (#428)

#### make NaN == NaN inside Expression (#318)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.30.0-rc.0 (2025-01-10)

### Breaking Changes

#### make `mut` lowercase-only and add it to `ReservedKeyword`

#### only parse keywords if they form the whole identifier

#### don't parse `NaN`, `inf`, and `infinity` as floats

#### tokenize keywords and identifiers correctly (#428)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.29.3-rc.0 (2025-01-09)

### Features

#### add missing `__all__` definitions to `.pyi` files

## 0.29.2 (2024-11-06)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

### Fixes

#### box nested ExternError variants (#419)

## 0.29.2-rc.2 (2024-11-06)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

### Fixes

#### box nested ExternError variants (#419)

## 0.29.2-rc.1 (2024-11-01)

### Features

#### add source mapping for calibration expansion (#370)

#### support inspection of ExternSignatureMap (#417)

## 0.29.2-rc.0 (2024-10-12)

### Features

#### add source mapping for calibration expansion (#370)

## 0.29.1 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.29.1-rc.0 (2024-10-10)

### Fixes

#### use correct Numpy version (and bump version number) (#416)

## 0.29.0 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.29.0-rc.2 (2024-10-07)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

#### ensure extern signature map is publicly accessible

#### ensure extern signature map is publicly accessible (#410)

## 0.29.0-rc.1 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.29.0-rc.0 (2024-09-30)

### Breaking Changes

#### correctly compute duration for `erfsquare` waveform templates

### Features

#### support underscores in `erfsquare`, `padleft`, and `padright`

#### check for `pad_left` and `pad_right` on all waveforms, include `_` in `erf_square`

### Fixes

#### fix computation of duration for `erfsquare` waveform templates

## 0.28.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.28.1-rc.1 (2024-09-16)

### Fixes

#### update lexical

#### Update lexical-core

#### update Knope, get it to release an update

## 0.28.1-rc.0

### Fixes

- Update lexical-core

## 0.28.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.28.0-rc.0

### Breaking Changes

- Use 4 spaces for indentation. (#390)

## 0.27.3

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.27.3-rc.0

### Fixes

- derive Clone for ParseProgramError and lower-level errors (#383)

## 0.27.2

### Features

- allow using instruction handler when simplifying program (#395)

## 0.27.2-rc.0

### Features

- allow using instruction handler when simplifying program (#395)

## 0.27.1

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.27.1-rc.0

### Features

- Initialize `Instruction`s from a Quil string. Python `Instruction`s support the `pickle` module. (#382)

## 0.27.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.27.0-rc.1

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.27.0-rc.0

### Breaking Changes

- change Rust representation of classical instructions (#376)

### Features

- make all unit-only enums Copy (#377)

### Fixes

- update Python types to new representation of classical instructions
- update tests to work with new classical instruction types

## 0.26.2-rc.0

### Features

- make all unit-only enums Copy (#377)

## 0.26.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.26.1-rc.1

### Features

- add waveform templates (#369)

### Fixes

- Parsing programs with integers that overflow a u64 will no longer panic; instead, they will raise an error. (#372)

## 0.26.1-rc.0

### Features

- add waveform templates (#369)

## 0.26.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.26.0-rc.0

### Breaking Changes

- reduce number of classical instruction edges in InstructionBlock::graph

## 0.25.1

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.25.1-rc.0

### Features

- Support constructing ControlFlowGraph and BasicBlocks. (#359)

## 0.25.0

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.25.0-rc.0

### Breaking Changes

- Program instruction iteration and serialization is deterministic. (#355)

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.24.1-rc.0

### Fixes

- Program equality is sensitive to the order of calibration instructions. (#357)

## 0.24.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)

## 0.24.0-rc.0

### Breaking Changes

- CalibrationSet's and Program's will be considered equal if they contain the same set of calibrations, regardless of order. (#352)

## 0.23.0

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)

## 0.23.0-rc.1

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

### Fixes

- Revert "match exactly one qubit for DELAYs without frame specifier" (#342)

## 0.23.0-rc.0

### Breaking Changes

- #334: program scheduling and analysis utilities (#336)

## 0.22.6

### Fixes

- include separators between DEFCIRCUIT parameters (#338)

## 0.22.6-rc.0

### Fixes

- include separators between DEFCIRCUIT parameters (#338)

## 0.22.5

### Fixes

- `is_quil_t()` now correctly returns false for WAIT instructions (#331)

## 0.22.5-rc.0

### Fixes

- `is_quil_t()` now correctly returns false for WAIT instructions (#331)

## 0.22.4

### Fixes

- The `wrap_in_loop` method now applies the end target to the program (#329)

## 0.22.4-rc.0

### Fixes

- The `wrap_in_loop` method now applies the end target to the program (#329)

## 0.22.3

### Features

- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

## 0.22.3-rc.1

### Features

- Add methods for identifying Quil-T instructions and filtering instructions from `Program`s (#323)
- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

## 0.22.3-rc.0

### Features

- Add `Program.wrap_in_loop()` method (#321)
- Add methods for identifying Quil-T instructions and filtering instructions from Programs

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
