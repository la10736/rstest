[![Crate][crate-image]][crate-link]
[![Status][test-action-image]][test-action-link]
[![Apache 2.0 Licensed][license-apache-image]][license-apache-link]
[![MIT Licensed][license-mit-image]][license-mit-link]

# Helper crate for testing `rstest` crate

This crate proivides a little framework for wrinting end to end test. You can crete rust
project, add workspace modules and code file. Run `build` and `test` actions and write
assertions based on passed and failed tests.

Moreover give some utilities on assertions, strings manipulations and test name.

This crate isn't designed for general pourpuse use but just as `rstest` develop helper crate.


## License

Licensed under either of

* Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
[license-apache-link])

* MIT license [LICENSE-MIT](LICENSE-MIT) or [license-MIT-link]
at your option.

[//]: # (links)

[crate-image]: https://img.shields.io/crates/v/rstest_test.svg
[crate-link]: https://crates.io/crates/rstest_test
[test-action-image]: https://github.com/la10736/rstest/workflows/Test/badge.svg
[test-action-link]: https://github.com/la10736/rstest/actions?query=workflow:Test
[license-apache-image]: https://img.shields.io/badge/license-Apache2.0-blue.svg
[license-mit-image]: https://img.shields.io/badge/license-MIT-blue.svg
[license-apache-link]: http://www.apache.org/licenses/LICENSE-2.0
[license-MIT-link]: http://opensource.org/licenses/MIT
