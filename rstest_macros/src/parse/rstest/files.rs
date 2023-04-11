use std::{env, path::PathBuf};

use glob::glob;
use quote::ToTokens;
use syn::{parse_quote, visit_mut::VisitMut, Expr, FnArg, Ident, ItemFn, LitStr};

use crate::{
    error::ErrorsVec,
    parse::{
        extract_argument_attrs,
        vlist::{Value, ValueList},
    },
    utils::attr_is,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FilesGlobReferences(String);

impl FilesGlobReferences {
    fn error(&self, msg: &str) -> syn::Error {
        syn::Error::new_spanned(&self.0, msg)
    }
}

impl From<LitStr> for FilesGlobReferences {
    fn from(value: LitStr) -> Self {
        Self(value.value())
    }
}

pub(crate) fn extract_files(
    item_fn: &mut ItemFn,
) -> Result<Vec<(Ident, FilesGlobReferences)>, ErrorsVec> {
    let mut extractor = ValueFilesExtractor::default();
    extractor.visit_item_fn_mut(item_fn);
    extractor.take()
}

/// Simple struct used to visit function attributes and extract future args to
/// implement the boilerplate.
#[derive(Default)]
struct ValueFilesExtractor {
    files: Vec<(Ident, FilesGlobReferences)>,
    errors: Vec<syn::Error>,
}

impl ValueFilesExtractor {
    pub(crate) fn take(self) -> Result<Vec<(Ident, FilesGlobReferences)>, ErrorsVec> {
        if self.errors.is_empty() {
            Ok(self.files)
        } else {
            Err(self.errors.into())
        }
    }
}

impl VisitMut for ValueFilesExtractor {
    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        if matches!(node, FnArg::Receiver(_)) {
            return;
        }
        match extract_argument_attrs(
            node,
            |a| attr_is(a, "files"),
            |attr, name| attr.parse_args::<LitStr>().map(|s| (attr, name.clone(), s)),
        )
        .collect::<Result<Vec<_>, _>>()
        {
            Ok(files) => match files.len().cmp(&1) {
                std::cmp::Ordering::Equal => self.files.push({
                    let (_, name, s) = files.into_iter().next().unwrap();
                    (name, s.into())
                }),
                std::cmp::Ordering::Greater => {
                    self.errors
                        .extend(files.iter().skip(1).map(|(attr, _name, _s)| {
                            syn::Error::new_spanned(
                                attr.into_token_stream(),
                                "Cannot use #[files] more than once.".to_owned(),
                            )
                        }));
                }
                std::cmp::Ordering::Less => {}
            },
            Err(e) => {
                self.errors.push(e);
            }
        };
    }
}

trait BaseDir {
    fn base_dir(&self) -> Result<PathBuf, std::env::VarError> {
        env::var("CARGO_MANIFEST_DIR").map(PathBuf::from)
    }
}

struct DefaultBaseDir;

impl BaseDir for DefaultBaseDir {}

trait GlobResolver {
    fn glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
        let pattern = pattern.as_ref();
        let globs = glob(pattern).map_err(|e| format!("glob failed for whole path `{pattern}` due {e}"))?;
        globs.into_iter()
            .map(|p| 
                p.map_err(|e| format!("glob failed for file due {e}"))
            )
            .map(|r| 
                r.and_then(|p| 
                    p.canonicalize().map_err(|e| format!("failed to canonicalize {} due {e}", p.display())))
                )
            .collect()
    }
}

struct DefaultGlobResolver;

impl GlobResolver for DefaultGlobResolver {}

pub(crate) struct ValueListFromFiles<'a> {
    base_dir: Box<dyn BaseDir + 'a>,
    g_resolver: Box<dyn GlobResolver + 'a>
}

impl<'a> Default for ValueListFromFiles<'a> {
    fn default() -> Self {
        Self { g_resolver: Box::new(DefaultGlobResolver), base_dir: Box::new(DefaultBaseDir) }
    }
}

impl<'a> ValueListFromFiles<'a> {
    pub fn to_value_list(
        &self,
        files: Vec<(Ident, FilesGlobReferences)>,
    ) -> Result<Vec<ValueList>, syn::Error> {
        files
            .into_iter()
            .map(|(arg, refs)| {
                self.file_list_values(refs)
                    .map(|values| ValueList { arg, values })
            })
            .collect::<Result<Vec<ValueList>, _>>()
    }

    fn file_list_values(&self, refs: FilesGlobReferences) -> Result<Vec<Value>, syn::Error> {
        let base_dir = self.base_dir.base_dir()
            .map_err(|_| 
                refs.error("Rstest's #[files(...)] requires that CARGO_MANIFEST_DIR is defined to define glob the relative path")
            )?;
        let resolved_path = base_dir.join(&PathBuf::try_from(&refs.0).unwrap());
        let pattern = resolved_path.to_string_lossy();

        let paths = self.g_resolver.glob(pattern.as_ref())
            .map_err(|msg| refs.error(&msg))?;
        let mut values: Vec<(Expr, String)> = vec![];
        for abs_path in paths {
            let path_name = abs_path.strip_prefix(&base_dir).unwrap();

            let path_str = abs_path.to_string_lossy();
            values.push((
                parse_quote! {
                    <PathBuf as std::str::FromStr>::from_str(#path_str).unwrap()
                },
                path_name.to_string_lossy().to_string(),
            ));
        }

        if values.is_empty() {
            Err(refs.error("No file found"))?;
        }

        Ok(values
            .into_iter()
            .map(|(e, desc)| Value::new(e, Some(desc)))
            .collect())
    }
}

#[cfg(test)]
mod should {
    use super::*;
    use crate::test::{assert_eq, *};
    use rstest_test::assert_in;

    #[rstest]
    #[case::simple(r#"fn f(#[files("some_glob")] a: PathBuf) {}"#, "fn f(a: PathBuf) {}", &[("a", "some_glob")])]
    #[case::more_than_one(
        r#"fn f(#[files("first")] a: PathBuf, b: u32, #[files("third")] c: PathBuf) {}"#,
        r#"fn f(a: PathBuf, 
                b: u32, 
                c: PathBuf) {}"#,
        &[("a", "first"), ("c", "third")],
    )]
    fn extract(
        #[case] item_fn: &str,
        #[case] expected: &str,
        #[case] expected_files: &[(&str, &str)],
    ) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let files = extract_files(&mut item_fn).unwrap();

        assert_eq!(expected, item_fn);
        assert_eq!(
            files,
            expected_files
                .into_iter()
                .map(|(id, a)| (ident(id), FilesGlobReferences(a.to_string())))
                .collect::<Vec<_>>()
        );
    }

    #[rstest]
    #[case::no_more_than_one(
        r#"fn f(#[files("a")] #[files("b")] a: PathBuf) {}"#,
        "more than once"
    )]
    #[case::no_arg("fn f(#[files] a: PathBuf) {}", "#[files(...)]")]
    #[case::invalid_inner("fn f(#[files(a::b::c)] a: PathBuf) {}", "string literal")]
    fn raise_error(#[case] item_fn: &str, #[case] message: &str) {
        let mut item_fn: ItemFn = item_fn.ast();

        let err = extract_files(&mut item_fn).unwrap_err();

        assert_in!(format!("{:?}", err), message);
    }

    struct FakeBaseDir(PathBuf);
    impl From<&str> for FakeBaseDir {
        fn from(value: &str) -> Self {
            Self(PathBuf::from(value))
        }
    }
    impl BaseDir for FakeBaseDir {
        fn base_dir(&self) -> Result<PathBuf, std::env::VarError> {
            Ok(self.0.clone())
        }
    }

    impl<'a> ValueListFromFiles<'a> {
        fn new(bdir: impl BaseDir + 'a , g_resover: impl GlobResolver + 'a) -> Self {
            Self { base_dir: Box::new(bdir), g_resolver: Box::new(g_resover) }
        }
    }

    struct FakeResolver(Vec<String>);

    impl From<&[&str]> for FakeResolver {
        fn from(value: &[&str]) -> Self {
            Self(value.iter().map(ToString::to_string).collect())
        }
    }

    impl GlobResolver for FakeResolver {
        fn glob(&self, _pattern: &str) -> Result<Vec<PathBuf>, String> {
            Ok(self.0.iter().map(PathBuf::from).collect())
        }
    }

    #[test]
    fn generate_a_var_with_the_glob_resolved_path() {
        let bdir = "/base";
        let fresolver = FakeResolver::from(["/base/first", "/base/second"].as_slice());
        let values = ValueListFromFiles::new(FakeBaseDir::from(bdir), fresolver).to_value_list(
            vec![(ident("a"),FilesGlobReferences("no_mater".to_string()))]
        ).unwrap();

        let mut expected = vec![values_list("a", &[
            r#"<PathBuf as std::str::FromStr>::from_str("/base/first").unwrap()"#,
            r#"<PathBuf as std::str::FromStr>::from_str("/base/second").unwrap()"#,
            ])];
        expected[0].values[0].description = Some("first".into());
        expected[0].values[1].description = Some("second".into());

        assert_eq!(expected, values);
    }
}
