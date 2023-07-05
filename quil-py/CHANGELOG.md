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
