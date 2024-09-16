# TODO list

- [ ] Update rustup
- [ ] Update dependency `cargo upgrade`
- [ ] Run `cargo clippy`
- [ ] Update Release
  - [ ] `Cargo.toml` `rstest`
  - [ ] `Cargo.toml` `rstest_macros`
  - [ ] `README.md`
- [ ] Run all test: `for channel in stable beta nightly; do RSTEST_TEST_CHANNEL=${channel} cargo +${channel} test; done`
- [ ] Remove the `-dev` suffix
- [ ] Create docs and checks links
- [ ] Check CHANGELOG: **RELEASE DATE** and remove empty blocks
- [ ] Check README
- [ ] Create tag (Use github release)
- [ ] prepare deploy `cargo publish --dry-run`
- [ ] deploy `cargo publish`
- [ ] Prepare next changelog