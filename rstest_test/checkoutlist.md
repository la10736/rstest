# TODO list

- [ ] Update rustup
- [ ] Update dependency `cargo upgrade`
- [ ] Run all test
  - [ ] Stable: `RSTEST_TEST_CHANNEL=stable; cargo +stable test`
  - [ ] Beta: `RSTEST_TEST_CHANNEL=beta; cargo +beta test`
  - [ ] Nightly: `RSTEST_TEST_CHANNEL=nightly; cargo +nightly test`
- [ ] Check Cargo.toml version
- [ ] Check README
- [ ] prepare deploy `cargo publish --dry-run`
- [ ] deploy `cargo publish`
- [ ] Change next version
  - [ ] `Cargo.toml`
  - [ ] `README.md`)
- [ ] Change `rstest` dev-dependency version on `rstest_test`
