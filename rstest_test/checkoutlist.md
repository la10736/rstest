# TODO list

- [ ] Update rustup
- [ ] Update dependency `cargo upgrade`
- [ ] Run all test
  - [ ] Stable: `RSTEST_TEST_CHANNEL=stable; cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Beta: `RSTEST_TEST_CHANNEL=beta; cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Nightly: `RSTEST_TEST_CHANNEL=nightly; cargo +${RSTEST_TEST_CHANNEL} test`
- [ ] Check Cargo.toml version
- [ ] Check README
- [ ] prepare deploy `cargo publish --dry-run`
- [ ] deploy `cargo publish`
- [ ] Change next version
  - [ ] `Cargo.toml`
  - [ ] `README.md`)
- [ ] Change `rstest` and `rstest_reuse` dev-dependency version on `rstest_test`
