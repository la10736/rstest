# Changelog
## Unreleased

### Changed

- The `#[files(...)]` attribute now ignores matched directory paths by default.
  See [#306](https://github.com/la10736/rstest/pull/306) thanks to @Obito-git.

### Add

- Introduced the `#[dirs]` attribute, which can be used with `#[files(...)]` to explicitly include directory paths.
  See [#306](https://github.com/la10736/rstest/pull/306) thanks to @Obito-git.
- The CI now runs builds and tests on Windows, as well.

### Fixed

- Removed unsued trait and impl spotted out on `1.89.0-nightly`
- Add missed tests about ignore attribute's args in `rstest` expansion.
  See [#313](https://github.com/la10736/rstest/pull/313)
- The `#[files(...)]` attribute now works reliably on Windows.

## [0.25.0] 2025/3/2

### Changed

- Append generated test macro so next test macros are aware of it 
  (see [#291](https://github.com/la10736/rstest/pull/291) thanks to @kezhuw).

### Add

- Added a `#[mode = ...]` attribute to be used with the `#[files(...)]` attribute to change the way
  the files get passed to the test.
  (see [#295](https://github.com/la10736/rstest/issues/295) thanks to @lucascool12)

## [0.24.0] 2025/1/1

### Changed

- MSRV to 1.70.0 (see [#284](https://github.com/la10736/rstest/issues/284) thanks to @rnbguy)

### Add

- `#![no_std]` support: now you can use `rstest` also in `no_std` lib 
  (see [#282](https://github.com/la10736/rstest/issues/282) thanks to @rnbguy)
- `#[context]` to have test function name and other useful thighs on
  the tip of your fingers (see [#177](https://github.com/la10736/rstest/issues/177))

## [0.23.0] 2024/9/29

### Add

- You can now use environment variables in `#[files]` with an optional default value (see [#277](https://github.com/la10736/rstest/pull/277)).
- You can now set a base_dir for `#[files]` with the `$[base_dir = "..."]` attribute (see [#277](https://github.com/la10736/rstest/pull/277)).

## [0.22.0] 2024/8/4

### Changed

- Now it's possible destructuring input values both for cases, values and fixtures. See [#231](https://github.com/la10736/rstest/issues/231) for details

### Add

- Implemented `#[ignore]` attribute to ignore test parameters during fixtures resolution/injection. See [#228](https://github.com/la10736/rstest/issues/228) for details

### Fixed

- Lot of typo in code

## [0.21.0] 2024/6/1

### Changed

- Add feature `crate-name` enabled by default to opt-in crate rename
  support. See [#258](https://github.com/la10736/rstest/issues/258)

## [0.20.0] 2024/5/30

### Add

- Implemented `#[by_ref]` attribute to take get a local lifetime for test arguments.
  See [#241](https://github.com/la10736/rstest/issues/241) for more details. Thanks to
  @narpfel for suggesting it and useful discussions.
- Support for import `rstest` with another name. See [#221](https://github.com/la10736/rstest/issues/221)

### Fixed

- Don't remove Lifetimes from test function if any. See [#230](https://github.com/la10736/rstest/issues/230)
  [#241](https://github.com/la10736/rstest/issues/241) for more details.
- [`PathBuf`](https://doc.rust-lang.org/std/path/struct.PathBuf.html) does no longer need to be
  in scope when using `#[files]` (see [#242](https://github.com/la10736/rstest/pull/242))
- `#[from(now::accept::also::path::for::fixture)]` See [#246](https://github.com/la10736/rstest/issues/246)
  for more details

## [0.19.0] 2024/4/9

### Changed

- Defined `rust-version` for each crate (see [#227](https://github.com/la10736/rstest/issues/227))

### Fixed

- `#[once]` fixtures now require the returned type to be
  [`Sync`](https://doc.rust-lang.org/std/marker/trait.Sync.html) to prevent UB
  when tests are executed in parallel. (see [#235](https://github.com/la10736/rstest/issues/235)
  for more details)

- `#[future(awt)]` and `#[awt]` now properly handle mutable (`mut`) parameters by treating futures as immutable and
  treating the awaited rebinding as mutable.

## [0.18.2] 2023/8/13

### Changed

- Now `#[files]` accept also parent folders (see [#205](https://github.com/la10736/rstest/issues/205)
  for more details).

## [0.18.1] 2023/7/5

### Fixed

- Wrong doc test
- Docs

## [0.18.0] 2023/7/4

### Add

- Add support for `RSTEST_TIMEOUT` environment variable to define a max timeout
  for each function (see [#190](https://github.com/la10736/rstest/issues/190) for details).
  Thanks to @aviramha for idea and PR
- `#[files("glob path")]` attribute to generate tests based on files that
  satisfy the given glob path (see [#163](https://github.com/la10736/rstest/issues/163) for details).

### Changed

- Switch to `syn` 2.0 and edition 2021 : minimal Rust version now is 1.56.0
  both for `rstest` and `rstest_reuse` (see [#187](https://github.com/la10736/rstest/issues/187))

### Fixed

- Fixed wired behavior on extraction `#[awt]` function attrs (See
  [#189](https://github.com/la10736/rstest/issues/189))

## [0.17.0] 2023/3/19

### Add

- Add `#[awt]` and `#[future(awt)]` to `.await` future input

### Fixed

- Fixed wrong message when timeout tests panic before timeout expire (See #171)

## [0.16.0] 2022/11/27

### Changed

- Show `TEST START` banner only when trace some argument: See #158 for details.
- Add values to test name: See #160 for details.

### Fixed

- Updated test fixtures to 1.64.0 compiler's error messages.

## [0.15.0] 2022/06/27

### Fixed

- Timeout not compile if one of its test arguments il not a copy
  type [see #154]

## [0.14.0] 2022/06/19

### Changed

- Feature gated async timeout via `async-timeout` feature [see #148]

### Fixed

- Removed `async-std` build dependency [see #148]

## [0.13.0] 2022/05/15

### Add

- `#[timeout(duration)]` test implementation for both sync and async tests (See #136)

### Changed

- Split rstest in separated crates for macro and libs (See #32)

## [0.12.0] 2021/12/12

### Add

- Add `#[once]` fixture attribute to create static fixtures (See #119)

### Fixed

- Fixed check of available features before to enable macro diagnostic (See #126)

## [0.11.0] 2021/08/01

### Fixed

- use mutable fixture in in cases and value list (See #121)

## [0.10.0] 2021/05/16

### Add

- Rename fixture (See #107 and #108)

### Fixed

- Wired behaviour in `#[fixture]` with generics types that have transitive
  reference (See #116)

## [0.9.0] 2021/05/2

### Add

- `#[future]` arg attribute to remove `impl Future<>` boilerplate. (See #98)

## [0.8.0] 2021/4/25

### Add

- Magic Conversion: use literal string for define values where type implements
  `FromStr` trait (See #111)

### Changed

- `#[default]` arg attribute cannot use key = arbitrary rust expression syntax
  (is unstable https://github.com/rust-lang/rust/issues/78835). So we switched
  to `#[default(expression)]` syntax. (See #117)

### Fixed

- #117 introduced an unstable syntax

## [0.7.0] 2021/3/21

This version intruduce the new more composable syntax. And async
fixtures (thanks to @rubdos)

### Add

- New syntax that leverage on function and argument attributes
  to implement all features (See #99, #100, #101, #103, #109 and #102)
- `async` fixtures (See #86, #96. Thanks to @rubdos).

### Changed

- Moved integration tests resouces in `test` directory (See #97)

## [0.6.4] 2020/6/20

### Add

- Implemented reusable test list with `rstest_reuse` external crate (See #80)

## [0.6.3] 2020/4/19

### Add

- Define default values instead use trivial fixtures (See #72).

## [0.6.2] 2020/4/13

### Add

- Injecting test attribute. You can choose your own test attribute (should be something like `*::test`)
  for each test. This feature enable every async runtime (See #91).

### Changed

- Start to use `rstest` to test `rstest` (On going task #92)

## [0.6.1] 2020/4/5

### Add

- Introducing async tests support. Leverage on `async_std::test` to automatically switch to
  async test (both for single, cases and matrix) (See #73)

## [0.6.0] 2020/3/5

### Add

- Hook argument name to fixture by remove starting `_` (See #70)
- Every `case` can have a specific set of attributes (See #82)

### Changed

- Removed useless `rstest_parametrize` and `rstest_matrix` (See #81). From 0.5.0 you
  can use just `rstest` to create cases and values list.
- Removed `cargo-edit` test dependecy (See #61)

## [0.5.3] 2020/1/23

### Fixed

- Fixed a false _unused mut_ warning regression introduced by
  partial fixtures (See [8a0ff08](https://github.com/la10736/rstest/commit/8a0ff0874dc8186edfaefb1ddef64d53666b94da))

## [0.5.2] 2019/12/29

### Fixed

- Fixed _unused attribute_ warning when use `should_panic`
  attribute (See #79)

## [0.5.1] 2019/12/14

### Fixed

- `README.md` links
- License files

## [0.5.0] 2019/12/13

### Added

- Use just `rstest` for implementing all kind of tests (See #42)
- New matrix tests render: indicate argument name and nest groups
  in modules (See #68 for details)
- CI (github actions) build and tests (See #46)

### Changed

- Better `README.md` that introduce all features
- (From rustc 1.40) Deprecated `rstest_parametrize` and `rstest_matrix`:
  `rstest` now cover all features
- Refactored

### Fixed

- Error message if fixture or value are used more than once

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

## [0.1.0] ...

Just my testing and private use.
