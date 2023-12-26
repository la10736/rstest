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

    fn read_file(path: &str) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

#[derive(Default)]
pub(crate) struct DefaultSysEngine;

impl SysEngine for DefaultSysEngine {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic(expected = "glob failed")]
    fn default_glob_resolver_raise_error_if_invalid_glob_path() {
        DefaultSysEngine::glob("/invalid/path/***").unwrap();
    }
}

#[cfg(test)]
pub(crate) mod mock {
    use std::path::PathBuf;

    use super::MockSysEngine;

    pub(crate) struct Context {
        pub(crate) cr: super::__mock_MockSysEngine_SysEngine::__crate_root::Context,
        pub(crate) g: super::__mock_MockSysEngine_SysEngine::__glob::Context,
        pub(crate) r: super::__mock_MockSysEngine_SysEngine::__read_file::Context,
    }

    impl Context {
        pub fn expected_crate_root(self, crate_root: PathBuf) -> Self {
            self.cr.expect().return_const(Ok(crate_root));
            self
        }

        pub fn expected_glob(self, query: impl ToString, globs: Vec<PathBuf>) -> Self {
            self.g
                .expect()
                .with(mockall::predicate::eq(query.to_string()))
                .return_const(Ok(globs));
            self
        }

        pub fn expected_file_context(self, s: &[(&str, &str)]) -> Self {
            for &(path, content) in s {
                let content = content.to_string();
                self.r
                    .expect()
                    .with(mockall::predicate::eq(path.to_string()))
                    .returning(move |_| Ok(content.to_string()));
            }
            self
        }
    }

    impl Default for Context {
        fn default() -> Self {
            Self {
                cr: MockSysEngine::crate_root_context(),
                g: MockSysEngine::glob_context(),
                r: MockSysEngine::read_file_context(),
            }
        }
    }
}
