use rstest::*;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[rstest]
fn start_with_name(
    #[files("files/*.txt")]
    #[base_dir = "${BASE_DIR:-}"]
    path: PathBuf,
) {
    let name = path.file_name().unwrap();
    let mut f = File::open(&path).unwrap();
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();

    assert!(contents.starts_with(name.to_str().unwrap()))
}
