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
pub(crate) mod mock {
    use std::path::PathBuf;

    use super::MockSysEngine;

    pub(crate) fn expected_crate_root(
        crate_root: PathBuf,
    ) -> super::__mock_MockSysEngine_SysEngine::__crate_root::Context {
        let cr_ctx = MockSysEngine::crate_root_context();
        cr_ctx.expect().return_const(Ok(crate_root));
        cr_ctx
    }

    pub(crate) fn expected_glob(
        query: impl ToString,
        globs: Vec<PathBuf>,
    ) -> super::__mock_MockSysEngine_SysEngine::__glob::Context {
        let g_ctx = MockSysEngine::glob_context();
        g_ctx
            .expect()
            .with(mockall::predicate::eq(query.to_string()))
            .return_const(Ok(globs));
        g_ctx
    }

    pub(crate) fn expected_file_context(
        s: &[(&str, &str)],
    ) -> super::__mock_MockSysEngine_SysEngine::__read_file::Context {
        let r_ctx = MockSysEngine::read_file_context();
        for &(path, content) in s {
            let content = content.to_string();
            r_ctx
                .expect()
                .with(mockall::predicate::eq(path.to_string()))
                .returning(move |_| Ok(content.to_string()));
        }
        r_ctx
    }
}
