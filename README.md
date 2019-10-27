[![Crate][crate-image]][crate-link]
[![Docs][docs-image]][docs-link]
[![Apache 2.0 Licensed][license-apache-image]][license-apache-link]
[![MIT Licensed][license-mit-image]][license-mit-link]
![Rust 1.32+][rustc-image]

# Fixture-based test framework for Rust

`rstest` uses procedural macros to implement simple fixtures and table-based tests.
To use it, add the following lines to your `Cargo.toml` file:

```
[dev-dependencies]
rstest = "0.4"
```

The core idea is that every input arguments of your test function will
be resolved by call a function with the same name.
Example:

```rust
use rstest::*;

#[fixture]
pub fn fixture() -> u32 { 42 }

#[rstest]
fn should_success(fixture: u32) {
    assert_eq!(fixture, 42);
}

#[rstest]
fn should_fail(fixture: u32) {
    assert_ne!(fixture, 42);
}
```

Moreover you can use `rstest_parametrize` to implement table-based tests.
You must indicate the arguments that you want use in your cases and provide them for each case you want to test.

`rstest_parametrize` generates an independent test for each case.

```rust
# use rstest::rstest_parametrize;
#[rstest_parametrize(input, expected,
    case(0, 0),
    case(1, 1),
    case(2, 1),
    case(3, 2),
    case(4, 3)
)]
fn fibonacci_test(input: u32, expected: u32) {
    assert_eq!(expected, fibonacci(input))
}
```

Running `cargo test` in this case executes five tests:

```bash
running 5 tests
test fibonacci_test::case_1 ... ok
test fibonacci_test::case_2 ... ok
test fibonacci_test::case_3 ... ok
test fibonacci_test::case_4 ... ok
test fibonacci_test::case_5 ... ok

test result: ok. 5 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

You can learn more on [Docs](https://docs.rs/rstest) and find more examples in [`resources`](resources) directory and in 
[`rs8080`](https://github.com/la10736/rs8080/blob/master/src/cpu/test.rs) 
which uses this module in-depth.

## Changelog

See [Changelog][CHANGELOG.md]

## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or 
[license-apache-link])

* MIT license ([LICENSE-MIT](LICENSE-MIT) or [license-MIT-link])
at your option.

[//]: # (links)

[crate-image]: https://img.shields.io/crates/v/rstest.svg
[crate-link]: https://crates.io/crates/rstest
[docs-image]: https://docs.rs/rstest/badge.svg
[docs-link]: https://docs.rs/rstest/
[license-apache-image]: https://img.shields.io/badge/license-Apache2.0-blue.svg
[license-mit-image]: https://img.shields.io/badge/license-MIT-blue.svg
[license-apache-link]: http://www.apache.org/licenses/LICENSE-2.0
[license-MIT-link]: http://opensource.org/licenses/MIT
[rustc-image]: https://img.shields.io/badge/rustc-1.32+-blue.svg
