# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

rstest is a fixture-based test framework for Rust, implemented as procedural macros. It provides dependency injection, parametrized tests, matrix tests, async support, and test timeouts.

## Workspace Layout

Cargo workspace with four member crates:
- **rstest_macros** — proc-macro crate: parses `#[fixture]` and `#[rstest]` attributes, generates test code. This is where most logic lives.
- **rstest** — main library crate users depend on. Re-exports macros and provides runtime support (timeout, magic conversion, context).
- **rstest_reuse** — separate proc-macro for `#[template]`/`#[apply]` test case reuse.
- **rstest_test** — internal testing utilities (project scaffolding, output assertions) used by the other crates' tests.

Excluded from workspace: `playground` (manual testing), `rstest_fixtures`.

## Build & Test Commands

```bash
cargo build --all              # build everything
cargo test --all               # run all tests
cargo test -p rstest_macros    # test a single crate
cargo test -p rstest -- <name> # run a specific test by name
cargo fmt --all -- --check     # check formatting
cargo clippy --all             # lint
```

CI tests against stable, beta, and nightly by setting `RSTEST_TEST_CHANNEL` env var:
```bash
RSTEST_TEST_CHANNEL=stable cargo test --all --verbose
```

MSRV checking (requires `cargo-hack`):
```bash
cargo hack check --rust-version --workspace --ignore-private
cargo hack test --rust-version --workspace --ignore-private -- --skip rstest::ignore_attributes_args_if_any
```

## Architecture: rstest_macros

The proc-macro pipeline follows a **parse → resolve → render** pattern:

1. **Parse** (`src/parse/`): Extracts fixture/test metadata from proc-macro attributes. Key types: `FixtureInfo`, `RsTestInfo`, `RsTestData`. Submodules handle specific attribute kinds (`arguments.rs`, `vlist.rs`, `future.rs`, `testcase.rs`, `files.rs`).

2. **Resolve** (`src/resolver.rs`): Resolves fixture dependencies and default values.

3. **Render** (`src/render/`): Generates output TokenStreams. Routes to `single()`, `parametrize()`, or `matrix()` based on parsed test type. `render/fixture.rs` handles fixture generation, `render/test.rs` handles test generation (largest file).

Entry points in `lib.rs`: `#[proc_macro_attribute] fixture()` and `#[proc_macro_attribute] rstest()`.

**Build script** (`build.rs`): Detects Rust channel to conditionally enable `proc_macro_diagnostic` on nightly.

## Testing Approach

Integration tests compile and run small Rust projects as subprocesses. The `rstest_test` crate provides `Project` for scaffolding temp projects and utilities for asserting on compiler/test output. Test resource files live in `rstest/tests/resources/`.

MSRV is 1.85. CI runs on Ubuntu and Windows.
