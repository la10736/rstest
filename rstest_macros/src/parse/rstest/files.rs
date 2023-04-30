use std::{env, path::PathBuf};

use glob::glob;
use quote::{ToTokens, format_ident};
use regex::Regex;
use relative_path::RelativePath;
use syn::{parse_quote, visit_mut::VisitMut, Expr, FnArg, Ident, ItemFn, LitStr, parse::Parse, Token, parenthesized};

use crate::{
    error::ErrorsVec,
    parse::{
        extract_argument_attrs,
        vlist::{Value, ValueList},
    },
    utils::attr_is,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FilesGlobReferences(LitStr, Option<Exclude>);

impl Parse for FilesGlobReferences {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        let exclude = 
            input.parse::<Option<Token![,]>>()?
                .map(|_comma|
                    input.parse::<Exclude>()
            ).transpose()?;
        Ok(Self(path, exclude))
    }
}

impl FilesGlobReferences {
    fn error(&self, msg: &str) -> syn::Error {
        syn::Error::new_spanned(&self.0, msg)
    }

    fn is_valid(&self, p: &RelativePath) -> bool {
        match self.1.as_ref() {
            Some(exclude) => !exclude.r.is_match(&p.to_string()),
            None => true,
        }
    }
}

#[derive(Debug, Clone)]
struct Exclude {
    s: LitStr,
    r: Regex,
}


impl PartialEq for Exclude {
    fn eq(&self, other: &Self) -> bool {
        self.s == other.s
    }
}

impl Parse for Exclude {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<Ident>()
            .and_then(
                |name| if name == format_ident!("exclude") {
                    Ok(())
                } else {
                    Err(syn::Error::new_spanned(name, r#"Use exclude("<regex>") to exclude some patters"#))
                }
            )
            .and_then(
                |_| {
                    let content;
                    let _ = parenthesized!(content in input);
                    let s: LitStr = content.parse()?;
                    regex::Regex::new(&s.value())
                        .map_err(|e| syn::Error::new_spanned(&s, format!("Should be a valid regex: {}", e)))
                        .map(|r| 
                            Self {
                                s,
                                r
                            }
                        )
                }
            )
    }
}

impl From<LitStr> for FilesGlobReferences {
    fn from(value: LitStr) -> Self {
        Self(value, None)
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
            |attr, name| attr.parse_args::<FilesGlobReferences>().map(|s| (attr, name.clone(), s)),
        )
        .collect::<Result<Vec<_>, _>>()
        {
            Ok(files) => match files.len().cmp(&1) {
                std::cmp::Ordering::Equal => self.files.push({
                    let (_, name, s) = files.into_iter().next().unwrap();
                    (name, s)
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
    fn base_dir(&self) -> Result<PathBuf, String> {
        env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .map_err(|_| 
                "Rstest's #[files(...)] requires that CARGO_MANIFEST_DIR is defined to define glob the relative path".to_string()
            )
    }
}

struct DefaultBaseDir;

impl BaseDir for DefaultBaseDir {}

trait GlobResolver {
    fn glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
        let pattern = pattern.as_ref();
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
}

struct DefaultGlobResolver;

impl GlobResolver for DefaultGlobResolver {}

pub(crate) struct ValueListFromFiles<'a> {
    base_dir: Box<dyn BaseDir + 'a>,
    g_resolver: Box<dyn GlobResolver + 'a>,
}

impl<'a> Default for ValueListFromFiles<'a> {
    fn default() -> Self {
        Self {
            g_resolver: Box::new(DefaultGlobResolver),
            base_dir: Box::new(DefaultBaseDir),
        }
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
        let base_dir = self.base_dir.base_dir().map_err(|msg| refs.error(&msg))?;
        let resolved_path = 
            RelativePath::from_path(&refs.0.value())
            .map_err(|e| refs.error(&format!("Invalid glob path: {e}")))?
            .to_logical_path(&base_dir);
        let pattern = resolved_path.to_string_lossy();

        let paths = self
            .g_resolver
            .glob(pattern.as_ref())
            .map_err(|msg| refs.error(&msg))?;
        let mut values: Vec<(Expr, String)> = vec![];

        for abs_path in paths {
            let relative_path = abs_path.strip_prefix(&base_dir).unwrap();
            if !refs.is_valid(&RelativePath::from_path(relative_path)
                .map_err(|e| refs.error(&format!("Invalid glob path: {e}")))?) {
                continue;
            }           

            let path_str = abs_path.to_string_lossy();
            values.push((
                parse_quote! {
                    <PathBuf as std::str::FromStr>::from_str(#path_str).unwrap()
                },
                relative_path.to_string_lossy().to_string(),
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

    fn lit_str(lstr: impl AsRef<str>) -> LitStr {
        let lstr = lstr.as_ref();
        parse_quote!{
            #lstr
        }
    }

    impl From<&str> for Exclude {
        fn from(value: &str) -> Self {
            Self {
                s: lit_str(value),
                r: regex::Regex::new(value).unwrap()
            }
        }
    }

    #[rstest]
    #[case::simple(r#"fn f(#[files("some_glob")] a: PathBuf) {}"#, "fn f(a: PathBuf) {}", &[("a", "some_glob", None)])]
    #[case::more_than_one(
        r#"fn f(#[files("first")] a: PathBuf, b: u32, #[files("third")] c: PathBuf) {}"#,
        r#"fn f(a: PathBuf, 
                b: u32, 
                c: PathBuf) {}"#,
        &[("a", "first", None), ("c", "third", None)],
    )]
    #[case::exclude(r#"fn f(#[files("some_glob", exclude("exclude"))] a: PathBuf) {}"#, 
    "fn f(a: PathBuf) {}", &[("a", "some_glob", Some("exclude"))])]
    fn extract(
        #[case] item_fn: &str,
        #[case] expected: &str,
        #[case] expected_files: &[(&str, &str, Option<&str>)],
    ) {
        let mut item_fn: ItemFn = item_fn.ast();
        let expected: ItemFn = expected.ast();

        let files = extract_files(&mut item_fn).unwrap();

        assert_eq!(expected, item_fn);
        assert_eq!(
            files,
            expected_files
                .into_iter()
                .map(|(id, a, ex)| (ident(id), FilesGlobReferences(lit_str(a), 
                ex.map(|ex| ex.into()))))
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

    #[derive(Default)]
    struct FakeBaseDir(PathBuf);
    impl From<&str> for FakeBaseDir {
        fn from(value: &str) -> Self {
            Self(PathBuf::from(value))
        }
    }
    impl BaseDir for FakeBaseDir {
        fn base_dir(&self) -> Result<PathBuf, String> {
            Ok(self.0.clone())
        }
    }

    impl<'a> ValueListFromFiles<'a> {
        fn new(bdir: impl BaseDir + 'a, g_resover: impl GlobResolver + 'a) -> Self {
            Self {
                base_dir: Box::new(bdir),
                g_resolver: Box::new(g_resover),
            }
        }
    }

    #[derive(Default)]
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

    #[rstest]
    #[case::simple("/base", FakeResolver::from(["/base/first", "/base/second"].as_slice()), None, &["first", "second"])]
    #[case::exclude("/base", FakeResolver::from([
        "/base/first", "/base/rem_1", "/base/other/rem_2", "/base/second"].as_slice()), 
        Some(Exclude { s: lit_str("no_mater"), r: Regex::new("rem_").unwrap() }), &["first", "second"])]
    fn generate_a_variable_with_the_glob_resolved_path(
        #[case] bdir: &str, 
        #[case] resolver: impl GlobResolver,
        #[case] exclude: Option<Exclude>,
        #[case] expected: &[&str]
    ) {
        let values = ValueListFromFiles::new(FakeBaseDir::from(bdir), resolver)
            .to_value_list(vec![(
                ident("a"),
                FilesGlobReferences(lit_str("no_mater"), 
                exclude),
            )])
            .unwrap();

        let mut v_list = values_list("a", 
            &expected
                    .iter()
                    .map(|p| format!(r#"<PathBuf as std::str::FromStr>::from_str("{}/{}").unwrap()"#, bdir, p))
                    .collect::<Vec<_>>()
        );
        v_list.values.iter_mut()
            .zip(expected.iter())
            .for_each(
                |(v, &ex)| v.description = Some(ex.into())
            );
        assert_eq!(vec![v_list], values);
    }

    #[test]
    #[should_panic(expected = "Fake error")]
    fn raise_error_if_fail_to_get_root() {
        #[derive(Default)]
        struct ErrorBaseDir;
        impl BaseDir for ErrorBaseDir {
            fn base_dir(&self) -> Result<PathBuf, String> {
                Err("Fake error".to_string())
            }
        }

        ValueListFromFiles::new(ErrorBaseDir::default(), FakeResolver::default())
            .to_value_list(vec![(
                ident("a"),
                FilesGlobReferences(lit_str("no_mater"), None),
            )])
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "No file found")]
    fn raise_error_if_no_files_found() {
        ValueListFromFiles::new(FakeBaseDir::default(), FakeResolver::default())
            .to_value_list(vec![(
                ident("a"),
                FilesGlobReferences(lit_str("no_mater"), None),
            )])
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "glob failed")]
    fn default_glob_resolver_raise_error_if_invalid_glob_path() {
        DefaultGlobResolver.glob("/invalid/path/***").unwrap();
    }
}
