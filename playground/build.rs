pub fn main() {
    #[cfg(feature = "build-rs")]
    println!("cargo::rerun-if-env-changed=BASE_DIR");
}
