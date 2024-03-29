# TODO list

- [ ] Update rustup
- [ ] Update dependency `cargo upgrade`
- [ ] Run `cargo clippy`
- [ ] Update Release
  - [ ] `Cargo.toml` `rstest`
  - [ ] `Cargo.toml` `rstest_macros`
  - [ ] `README.md`
- [ ] Run all test
  - [ ] Stable: `RSTEST_TEST_CHANNEL=stable; cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Beta: `RSTEST_TEST_CHANNEL=beta; cargo +${RSTEST_TEST_CHANNEL} test`
  - [ ] Nightly: `RSTEST_TEST_CHANNEL=nightly; cargo +${RSTEST_TEST_CHANNEL} test`
- [ ] Create docs and checks links
- [ ] Check CHANGELOG: **RELEASE DATE** and remove empty blocks
- [ ] Check README
- [ ] Create tag (Use github release)
- [ ] prepare deploy `cargo publish --dry-run`
- [ ] deploy `cargo publish`
- [ ] Change dependency (inner `rstest` and `rstest_ruse`)
- [ ] Prepare next changelog