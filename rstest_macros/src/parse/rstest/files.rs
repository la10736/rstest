use std::{env, path::PathBuf};

use quote::ToTokens;
use syn::{visit_mut::VisitMut, FnArg, Ident, ItemFn, LitStr, Expr, parse_quote};
use glob::glob;

use crate::{
    error::ErrorsVec,
    parse::{extract_argument_attrs, vlist::{ValueList, Value}},
    utils::attr_is,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FilesGlobReferences(String);

impl From<LitStr> for FilesGlobReferences {
    fn from(value: LitStr) -> Self {
        Self(value.value())
    }
}

impl From<(Ident, FilesGlobReferences)> for ValueList {
    fn from(value: (Ident, FilesGlobReferences)) -> Self {
        let (arg, refs) = value;
        let base_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR")
            .expect("Rstest's #[files(...)] requires that CARGO_MANIFEST_DIR is defined to define glob the relative path"));
        let resolved_path = base_dir.join(&PathBuf::try_from(&refs.0).unwrap());
        let pattern = resolved_path.to_string_lossy();

        let paths =
            glob(&pattern).unwrap_or_else(|e| panic!("glob failed for whole path `{pattern}` due {e}"));
        let mut values: Vec<(Expr, String)> = vec![];
        for path in paths {
            let path = path.unwrap_or_else(|e| panic!("glob failed for file due {e}"));
            let abs_path = path
                .canonicalize()
                .unwrap_or_else(|e| panic!("failed to canonicalize {} due {e}", path.display()));
            let path_name = abs_path.strip_prefix(&base_dir).unwrap();

            let path_str = abs_path.to_string_lossy();
            values.push((parse_quote!{
                <PathBuf as std::str::FromStr>::from_str(#path_str).unwrap()
            }, path_name.to_string_lossy().to_string()));
        }

        if values.is_empty() {
            panic!("No file found")
        }

        Self {
            arg,
            values: values.into_iter().map(|(e, desc)| Value::new(e, Some(desc))).collect(),
        }
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
}
