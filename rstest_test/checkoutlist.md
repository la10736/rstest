# TODO list

- [ ] Update rustup
- [ ] Update dependency `cargo upgrade`
- [ ] Run all test
  - [ ] Stable: `RSTEST_TEST_CHANNEL=stable cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Beta: `RSTEST_TEST_CHANNEL=beta cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Nightly: `RSTEST_TEST_CHANNEL=nightly cargo +${RSTEST_TEST_CHANNEL} test`
- [ ] Change next version
  - [ ] `Cargo.toml`
- [ ] prepare deploy `cargo publish --dry-run`
- [ ] deploy `cargo publish`
