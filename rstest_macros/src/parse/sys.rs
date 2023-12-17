use glob::glob;
use std::{env, path::PathBuf};

/// This trait provide the interface for abstracting the functions that
/// need to use system resource
#[cfg_attr(test, mockall::automock)]
pub(crate) trait SysEngine {
    /// This function returns the absolute path of the crate root
    fn crate_root() -> Result<PathBuf, String> {
        env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .map_err(|_|
                "Rstest's #[files(...)] requires that CARGO_MANIFEST_DIR is defined to define glob the relative path".to_string()
            )
    }

    fn glob(pattern: &str) -> Result<Vec<PathBuf>, String> {
        let pattern = pattern;
        let globs =
            glob(pattern).map_err(|e| format!("glob failed for whole path `{pattern}` due {e}"))?;
        globs
            .into_iter()
            .map(|p| p.map_err(|e| format!("glob failed for file due {e}")))
            .map(|r| {
                r.and_then(|p| {
                    p.canonicalize()
                        .map_err(|e| format!("failed to canonicalize {} due {e}", p.display()))
                })
            })
            .collect()
    }

    fn read_file(path: &str) -> Result<String, String> {
        std::fs::read_to_string(path).map_err(|e| format!("failed to read file `{path}` due {e}"))
    }
}

#[derive(Default)]
pub(crate) struct DefaultSysEngine;

impl SysEngine for DefaultSysEngine {}
