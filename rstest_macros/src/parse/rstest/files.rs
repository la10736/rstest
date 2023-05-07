use std::{env, path::PathBuf};

use glob::glob;
use quote::{format_ident, ToTokens};
use regex::Regex;
use relative_path::RelativePath;
use syn::{
    parenthesized, parse::Parse, parse_quote, visit_mut::VisitMut, Expr, FnArg, Ident, ItemFn,
    LitStr, Token,
};

use crate::{
    error::ErrorsVec,
    parse::{
        extract_argument_attrs,
        vlist::{Value, ValueList},
    },
    utils::attr_is,
};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FilesGlobReferences {
    glob: Vec<LitStr>,
    exclude: Option<Exclude>,
    ignore_dot_files: bool,
}

impl Parse for FilesGlobReferences {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let glob = vec![input.parse()?];
        let exclude = input
            .parse::<Option<Token![,]>>()?
            .map(|_comma| input.parse::<Exclude>())
            .transpose()?;
        Ok(Self::new(glob, exclude, true))
    }
}

trait RaiseError: ToTokens {
    fn error(&self, msg: &str) -> syn::Error {
        syn::Error::new_spanned(&self, msg)
    }
}

impl RaiseError for LitStr {}

impl FilesGlobReferences {
    fn new(glob: Vec<LitStr>, exclude: Option<Exclude>, ignore_dot_files: bool) -> Self { Self { glob, exclude, ignore_dot_files } }

    fn is_valid(&self, p: &RelativePath) -> bool {
        if self.ignore_dot_files {
            if p.components().into_iter().any(|c| c.as_str().starts_with('.')) {
                return false;
            }
        }
        match self.exclude.as_ref() {
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
        input
            .parse::<Ident>()
            .and_then(|name| {
                if name == format_ident!("exclude") {
                    Ok(())
                } else {
                    Err(syn::Error::new_spanned(
                        name,
                        r#"Use exclude("<regex>") to exclude some patters"#,
                    ))
                }
            })
            .and_then(|_| {
                let content;
                let _ = parenthesized!(content in input);
                let s: LitStr = content.parse()?;
                regex::Regex::new(&s.value())
                    .map_err(|e| {
                        syn::Error::new_spanned(&s, format!("Should be a valid regex: {}", e))
                    })
                    .map(|r| Self { s, r })
            })
    }
}

impl From<Vec<LitStr>> for FilesGlobReferences {
    fn from(value: Vec<LitStr>) -> Self {
        Self::new(value, None, true)
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
            |attr, name| {
                attr.parse_args::<FilesGlobReferences>()
                    .map(|s| (attr, name.clone(), s))
            },
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
        let base_dir = self.base_dir.base_dir().map_err(|msg| refs.glob[0].error(&msg))?;
        let resolved_paths = 
            refs.glob.iter()
            .map(
                |g| 
                    RelativePath::from_path(&g.value())
                        .map_err(|e| g.error(&format!("Invalid glob path: {e}")))
                        .map(|p| p.to_logical_path(&base_dir))
                        .map(|p| (g, p.to_string_lossy().into_owned()))
            
            ).collect::<Result<Vec<_>,_>>()?;

        let mut paths = 
            resolved_paths.iter()
            .map(|(r, pattern)| 
                self
                    .g_resolver
                    .glob(pattern.as_ref())
                    .map_err(|msg| r.error(&msg))
                    .map(|p| (r, p))
            ).collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .flat_map(|(&r, inner)| inner.into_iter().map(move |p| (r, p)))
            .collect::<Vec<_>>();
        let mut values: Vec<(Expr, String)> = vec![];
        paths.sort_by(|(_, a),(_, b)| a.cmp(b));
        paths.dedup_by(|(_, a), (_, b)| a.eq(&b));
        for (r, abs_path) in paths {
            let relative_path = abs_path.strip_prefix(&base_dir).unwrap();
            if !refs.is_valid(
                &RelativePath::from_path(relative_path)
                    .map_err(|e| r.error(&format!("Invalid glob path: {e}")))?,
            ) {
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
            Err(refs.glob[0].error("No file found"))?;
        }

        Ok(values
            .into_iter()
            .map(|(e, desc)| Value::new(e, Some(desc)))
            .collect())
    }
}

#[cfg(test)]
mod should {
    use std::collections::HashMap;

    use super::*;
    use crate::test::{assert_eq, *};
    use rstest_test::assert_in;
    use maplit::hashmap;

    fn lit_str(lstr: impl AsRef<str>) -> LitStr {
        let lstr = lstr.as_ref();
        parse_quote! {
            #lstr
        }
    }

    impl From<&str> for Exclude {
        fn from(value: &str) -> Self {
            Self {
                s: lit_str(value),
                r: regex::Regex::new(value).unwrap(),
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
                .map(|(id, a, ex)| (
                    ident(id),
                    FilesGlobReferences::new(vec![lit_str(a)], ex.map(|ex| ex.into()), true)
                ))
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

    #[derive(Default)]
    struct FakeMapResolver(String, HashMap<String, Vec<PathBuf>>);

    impl From<(&str, &HashMap<&str, &[&str]>)> for FakeMapResolver {
        fn from(value: (&str, &HashMap<&str, &[&str]>)) -> Self {
            Self(
                value.0.to_string(),
                value.1.iter().map(
                    |(&key, &values)|
                    (key.to_string(), values.iter()
                        .map(|&v| PathBuf::from(format!("{}/{v}", value.0))).collect::<Vec<_>>())
                ).collect()
            )
        }
    }

    impl GlobResolver for FakeMapResolver {
        fn glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
            let pattern = pattern.strip_prefix(&format!("{}/", self.0)).unwrap();
            Ok(self.1.get(pattern).cloned().unwrap_or_default())
        }
    }

    #[rstest]
    #[case::simple("/base", None, FakeResolver::from(["/base/first", "/base/second"].as_slice()), None, true, &["first", "second"])]
    #[case::more_glob("/base", Some(["path1", "path2"].as_slice()), FakeMapResolver::from(
        ("/base", &hashmap!(
            "path1" => ["first", "second"].as_slice(),
            "path2" => ["third", "zzzz"].as_slice()
        ))
    ), None, true, &["first", "second", "third", "zzzz"])]
    #[case::should_remove_duplicates("/base", Some(["path1", "path2"].as_slice()), FakeMapResolver::from(
        ("/base", &hashmap!(
            "path1" => ["first", "second"].as_slice(),
            "path2" => ["second", "third"].as_slice()
        ))
    ), None, true, &["first", "second", "third"])]
    #[case::should_sort("/base", None, FakeResolver::from(["/base/second", "/base/first"].as_slice()), None, true, &["first", "second"])]
    #[case::exclude("/base", None, FakeResolver::from([
        "/base/first", "/base/rem_1", "/base/other/rem_2", "/base/second"].as_slice()), 
        Some(Exclude { s: lit_str("no_mater"), r: Regex::new("rem_").unwrap() }), true, &["first", "second"])]
    #[case::ignore_dot_files("/base", None, FakeResolver::from([
        "/base/first", "/base/.ignore", "/base/.ignore_dir/a", "/base/second/.not", "/base/second/but_include", "/base/in/.out/other/ignored"].as_slice()), 
        None, true, &["first", "second/but_include"])]
    #[case::include_dot_files("/base", None, FakeResolver::from([
        "/base/first", "/base/.ignore", "/base/.ignore_dir/a", "/base/second/.not", "/base/second/but_include", "/base/in/.out/other/ignored"].as_slice()), 
        None, false, &[".ignore", ".ignore_dir/a", "first", "in/.out/other/ignored", "second/.not", "second/but_include"])]
    fn generate_a_variable_with_the_glob_resolved_path(
        #[case] bdir: &str,
        #[case] paths: Option<&[&str]>,
        #[case] resolver: impl GlobResolver,
        #[case] exclude: Option<Exclude>,
        #[case] ignore_dot_files: bool,
        #[case] expected: &[&str],
    ) {
        let paths = paths.map(
            |inner| inner.into_iter().map(lit_str).collect()
        ).unwrap_or(vec![lit_str("no_mater")]);
        let values = ValueListFromFiles::new(FakeBaseDir::from(bdir), resolver)
            .to_value_list(vec![(
                ident("a"),
                FilesGlobReferences::new(paths, exclude, ignore_dot_files),
            )])
            .unwrap();

        let mut v_list = values_list(
            "a",
            &expected
                .iter()
                .map(|p| {
                    format!(
                        r#"<PathBuf as std::str::FromStr>::from_str("{}/{}").unwrap()"#,
                        bdir, p
                    )
                })
                .collect::<Vec<_>>(),
        );
        v_list
            .values
            .iter_mut()
            .zip(expected.iter())
            .for_each(|(v, &ex)| v.description = Some(ex.into()));
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
                FilesGlobReferences::new(vec![lit_str("no_mater")], None, true),
            )])
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "No file found")]
    fn raise_error_if_no_files_found() {
        ValueListFromFiles::new(FakeBaseDir::default(), FakeResolver::default())
            .to_value_list(vec![(
                ident("a"),
                FilesGlobReferences::new(vec![lit_str("no_mater")], None, true),
            )])
            .unwrap();
    }

    #[test]
    #[should_panic(expected = "glob failed")]
    fn default_glob_resolver_raise_error_if_invalid_glob_path() {
        DefaultGlobResolver.glob("/invalid/path/***").unwrap();
    }
}
