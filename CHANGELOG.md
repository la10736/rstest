# Changelog

## [Unreleased]

## [0.4.1] 2019-10-05

### Changed

- Fixed README, crate description, changelog dates

## [0.4.0] 2019-10-04

### Added

- Injecting fixture with partial values in all tests (See #48)
- Add new `rstest_matrix` macro to build tests by carthesian product of 
input arguments (See #38)

### Fixed

- Just bugs in tests

### Changed

- Use `unindent` crate instead the home made `Deindent` trait
- Use `itertools`
- Refactor parsing

## [0.3.0] 2019-06-28

### Added

- Introduced `fixture` macro: Now you must annotate your fixture by
this tag. See #5
- Support for arbitrary rust code without use `Unwrap(str_lit)` trick. 
See #19 and #20 (deprecate `Unwrap()`)
- Support for tests that return `Result()`, See #23
- Support for dump test arguments
- `rstest_parametrize` use module to group cases (See #13)
- You can optionally give a an name for each test case (See #11)
- Docs
- Descriptive error handling: See #12, #15
- `rstest_parametrize` can leave comma after last case

### Fixed

- `rstest_parametrize` should catch error in input! See #1, #14
- Use negative literal. See #18

### Changed

- You need to use `fixture` to tag all your fixtures.
- Migrate to 2018 Epoch
- Tests: Refactoring and speed up

## [0.2.2] 2018-10-18

### Fixed

- Better error handling

## [0.2.1] 2018-10-15

### Changed

- crate.io categories

## [0.2.0] 2018-10-14

First Public release.

## [0.1.0] ....

Just my testing and private use.
