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
