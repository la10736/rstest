# TODO list

CI (`.github/workflows/test.yml`) already runs on every PR and push: tests on stable, beta and
nightly, msrv (`cargo hack check` and `cargo hack test`), `cargo fmt --all -- --check`,
`cargo clippy --all -- -D warnings`, `cargo doc --no-deps` with `RUSTDOCFLAGS="-D warnings"`
and `cargo publish --dry-run` of `rstest_macros` and `rstest_reuse`.

- [ ] Update Release
  - [ ] `README.md`
- [ ] Remove the `-dev` suffix, run `cargo test` after
- [ ] Check CHANGELOG: **RELEASE DATE** and remove empty blocks
- [ ] Check README
- [ ] **Commit** and push, check that CI is green on the release commit
- [ ] Create tag (Use github release)
- [ ] deploy:
  - [ ]  `cargo publish -p rstest_macros --dry-run`
  - [ ]  `cargo publish -p rstest_macros`
  - [ ]  `cargo publish -p rstest --dry-run`
  - [ ]  `cargo publish -p rstest`
  - [ ]  If `rstest_reuse` changed, release it too (see `rstest_reuse/checkoutlist.md`)
- [ ] Prepare next changelog
- [ ] New `-dev` version
- [ ] Update inner `rstest` reference
