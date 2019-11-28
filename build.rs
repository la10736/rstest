use rustc_version::{version, version_meta, Channel};

fn main() {
    let ver = version().unwrap();
    assert!(ver.major >= 1);

    match version_meta().unwrap().channel {
        Channel::Nightly | Channel::Dev => {
            println!("cargo:rustc-cfg=use_proc_macro_diagnostic");
        }
        _ => {}
    }
    if ver.major > 1 || ver.minor >= 40 {
        println!("cargo:rustc-cfg=deprecate_parametrize_matrix");
    }
}
