[![Crate][crate-image]][crate-link]
[![Status][test-action-image]][test-action-link]
[![Apache 2.0 Licensed][license-apache-image]][license-apache-link]
[![MIT Licensed][license-mit-image]][license-mit-link]
 
# Reuse `rstest`'s parametrized cases

This crate give a way to define a tests set and apply them to every case you need to
test. With `rstest` crate you can define a tests list but if you want to apply the same tests
to another test function you must rewrite all cases or write some macros that do the job.

Both solutions have some drawbreak:
- rewrite create duplication
- macros makes code harder to read and shift out the focus from tests core

The aim of this crate is solve this problem. `rstest_resuse` expose two attributes:
- `#[template]`: to define a template
- `#[apply]`: to apply a defined template to create create tests

Here is a simple example:

```rust
use rstest::rstest;
use rstest_reuse::{self, *};
// Here we define the template. This define
// * The test list name to `two_simple_cases`
// * cases: here two cases that feed the `a`, `b` values
#[template]
#[rstest(a,  b,
    case(2, 2),
    case(4/2, 2),
    )
]
fn two_simple_cases(a: u32, b: u32) {}
// Here we apply the `two_simple_cases` template: That is expanded in
// #[template]
// #[rstest(a,  b,
//     case(2, 2),
//     case(4/2, 2),
//     )
// ]
// fn it_works(a: u32, b: u32) {
//     assert!(a == b);
// }
#[apply(two_simple_cases)]
fn it_works(a: u32, b: u32) {
    assert!(a == b);
}
// Here we reuse the `two_simple_cases` template to create two other tests
#[apply(two_simple_cases)]
fn it_fail(a: u32, b: u32) {
    assert!(a != b);
}
```

if we run `cargo test` we have:

```text
    Finished test [unoptimized + debuginfo] target(s) in 0.05s
     Running target/debug/deps/playground-8a1212f8b5eb00ce
running 4 tests
test it_fail::case_1 ... FAILED
test it_works::case_1 ... ok
test it_works::case_2 ... ok
test it_fail::case_2 ... FAILED
failures:
---- it_fail::case_1 stdout ----
-------------- TEST START --------------
thread 'it_fail::case_1' panicked at 'assertion failed: a != b', src/main.rs:34:5
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
---- it_fail::case_2 stdout ----
-------------- TEST START --------------
thread 'it_fail::case_2' panicked at 'assertion failed: a != b', src/main.rs:34:5
failures:
    it_fail::case_1
    it_fail::case_2
test result: FAILED. 2 passed; 2 failed; 0 ignored; 0 measured; 0 filtered out
error: test failed, to rerun pass '--bin playground'
```

Simple and neat!

## Cavelets

### `use rstest_resuse` at the top of your crate
You **should** add `use rstest_resuse` at the top of your crate:

```rust
#[cfg(test)]
use rstest_reuse;
```

This is due `rstest_reuse::template` define a macro that need to call a `rstest_resuse`'s macro.
I hope to remove this in the future but for now we should live with it.

Note that

```rust
use rstest_reuse::*;
```
is not enougth: this statment doesn't include `rstest_reuse` but just its public items.

### Define `template` before `apply` it

`template` attribute define a macro that `apply` will use. Macro in rust are expanded in
a single depth-first, lexical-order traversal of a crate’s source, that means the template
definition should be allways before the `apply`.

### Tag modules with `#[macro_use]`

If you define a `template` in a module and you want to use it outside the module you should _lift_ it by
mark the module with the `#[macro_use]` attribute. This attribute make your `template` visibe outside this module
but not at the upper level. When a `template` is defined you can use it in all submodules that follow
the definition.

If you plan to spread your templates in some modules and you use a dirrerent name for each template
consider to add the global attribute `!#[macro_use]` at crate level: this put all your templates 
available everywhere: you should just take care that a `template` should be defined before the `apply` call.

## Disclamer

This crate is in developer stage. I don't know if I'll include it in `rstest` or changing some syntax in
the future.

I did't test it in a lot of cases: if you have some cases where it doesn't works file a ticket on [`rstest`][rstest-link]


## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
[license-apache-link])

* MIT license [LICENSE-MIT](LICENSE-MIT) or [license-MIT-link]
at your option.

[//]: # (links)

[crate-image]: https://img.shields.io/crates/v/rstest_reuse.svg
[crate-link]: https://crates.io/crates/rstest_reuse
[test-action-image]: https://github.com/la10736/rstest/workflows/Test/badge.svg
[test-action-link]: https://github.com/la10736/rstest/actions?query=workflow:Test
[license-apache-image]: https://img.shields.io/badge/license-Apache2.0-blue.svg
[license-mit-image]: https://img.shields.io/badge/license-MIT-blue.svg
[license-apache-link]: http://www.apache.org/licenses/LICENSE-2.0
[license-MIT-link]: http://opensource.org/licenses/MIT
[rstest-link]: https://github.com/la10736/rstest
