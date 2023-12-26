use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use syn::{spanned::Spanned, visit_mut::VisitMut, FnArg, Ident, ItemFn, LitStr};
use thiserror::Error;

use crate::{
    error::RaiseError,
    parse::{rstest::file, sys::SysEngine},
    refident::MaybeIdent,
    utils::attr_is,
};

use super::{attribute_args_once, file::Folder, maybe_parse_attribute_args_just_once};

#[derive(Error, Debug)]
pub(crate) enum HierarchyError {
    #[error("Cannot guess crate root due: {0}")]
    CrateRoot(String),
    #[error("Cannot get files from glob due: {0}")]
    InvalidGlob(String),
    #[error("Not a file error: {0}")]
    NotAFile(PathBuf),
    #[error("The file should be in a folder: '{0}'")]
    NotInFolder(PathBuf),
    #[error("Cannot read file '{path}' due: {source}")]
    ReadFileError {
        path: PathBuf,
        source: std::io::Error,
    },
    #[error("Parse error '{path}' due: {source}")]
    ParseError { path: PathBuf, source: ParseError },
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub(crate) struct StructField {
    ident: Ident,
    field: Option<String>,
}

impl StructField {
    pub(crate) fn new(ident: Ident, field: Option<String>) -> Self {
        Self { ident, field }
    }
}

impl quote::ToTokens for StructField {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ident.to_tokens(tokens);
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct JsonFiles {
    hierarchy: Hierarchy<JsonBody>,
    data: Vec<Ident>,
    args: Vec<StructField>,
}

impl JsonFiles {
    pub(crate) fn hierarchy(&self) -> &Hierarchy<JsonBody> {
        &self.hierarchy
    }

    pub(crate) fn data(&self) -> &[Ident] {
        self.data.as_ref()
    }

    pub(crate) fn args(&self) -> &[StructField] {
        self.args.as_ref()
    }
}

impl quote::ToTokens for JsonFiles {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.data.iter().for_each(|data| data.to_tokens(tokens));
        self.args.iter().for_each(|f| f.to_tokens(tokens));
    }
}

impl From<Hierarchy<JsonBody>> for JsonFiles {
    fn from(hierarchy: Hierarchy<JsonBody>) -> Self {
        Self {
            hierarchy,
            data: Default::default(),
            args: Default::default(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Hierarchy<T> {
    pub(crate) folder: Folder<T>,
}

impl<'a, T> From<&'a Path> for Hierarchy<T> {
    fn from(abs: &'a Path) -> Self {
        Self {
            folder: Folder::empty(abs),
        }
    }
}

impl<T> Hierarchy<T> {
    fn add_file_relative(
        &mut self,
        path: &std::path::Path,
        test_file: file::File<T>,
    ) -> Result<(), HierarchyError> {
        let parent = path
            .parent()
            .ok_or_else(|| HierarchyError::NotInFolder(path.to_path_buf()))?;

        self.folder.add_file_path(parent, test_file);
        Ok(())
    }
}

/// Simple struct used to visit function attributes and extract files and
/// eventually parsing errors
pub(crate) struct FilesExtractor<S> {
    pub(crate) hierarchy: Option<JsonFiles>,
    pub(crate) errors: Vec<syn::Error>,
    _phantom: std::marker::PhantomData<S>,
}

impl<S> Default for FilesExtractor<S> {
    fn default() -> Self {
        Self {
            hierarchy: Default::default(),
            errors: Default::default(),
            _phantom: Default::default(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum JsonBody {
    Root(String),
    Array(Vec<String>),
}

impl Default for JsonBody {
    fn default() -> Self {
        Self::Array(Vec::default())
    }
}

#[derive(Error, Debug)]
pub(crate) enum ParseError {
    #[error("Invalid json")]
    InvalidJson(#[from] json::JsonError),
    #[error("Should be a object or an array of object")]
    InvalidJsonData,
}

impl JsonBody {
    pub(crate) fn parse_json_str(json_str: &str) -> Result<Self, ParseError> {
        match json::parse(&json_str)? {
            json::JsonValue::Object(body) => Ok(Self::root(&json::stringify(body))),
            json::JsonValue::Array(array) if array.iter().all(|j| j.is_object()) => {
                let mut bodies = Self::array();
                for a in array {
                    bodies = bodies.add(&json::stringify(a));
                }
                Ok(bodies)
            }
            _ => Err(ParseError::InvalidJsonData),
        }
    }

    pub(crate) fn root(body: &str) -> Self {
        Self::Root(body.to_string())
    }

    pub(crate) fn array() -> Self {
        Self::Array(Default::default())
    }

    pub(crate) fn add(self, body: &str) -> Self {
        match self {
            Self::Root(b) => Self::Array(vec![b, body.to_string()]),
            Self::Array(mut bodies) => {
                bodies.push(body.to_string());
                Self::Array(bodies)
            }
        }
    }
}

impl<S: SysEngine> FilesExtractor<S> {
    fn build_hierarchy(&self, path: &LitStr) -> Result<Hierarchy<JsonBody>, HierarchyError> {
        let crate_root = S::crate_root().map_err(|m| HierarchyError::CrateRoot(m))?;
        let mut paths = S::glob(&path.value()).map_err(|e| HierarchyError::InvalidGlob(e))?;
        paths.sort();
        let mut hierarchy = Hierarchy::from(crate_root.as_path());
        for path in paths {
            let fname = path
                .file_name()
                .ok_or_else(|| HierarchyError::NotAFile(path.to_path_buf()))?;
            let data = S::read_file(&path.to_string_lossy()).map_err(|e| {
                HierarchyError::ReadFileError {
                    path: path.clone(),
                    source: e,
                }
            })?;
            let bodies =
                JsonBody::parse_json_str(&data).map_err(|e| HierarchyError::ParseError {
                    path: path.to_owned(),
                    source: e,
                })?;
            let test_file = file::File::empty(fname).content(bodies);

            hierarchy
                .add_file_relative(&path.strip_prefix(&crate_root).unwrap(), test_file)
                .unwrap();
        }
        Ok(hierarchy)
    }
}

impl<S: SysEngine> VisitMut for FilesExtractor<S> {
    fn visit_item_fn_mut(&mut self, node: &mut ItemFn) {
        let attrs = std::mem::take(&mut node.attrs);
        let mut attrs_buffer = Vec::<syn::Attribute>::default();
        for attr in attrs.into_iter() {
            if attr_is(&attr, "json") {
                match attr.parse_args::<LitStr>().and_then(|path| {
                    self.build_hierarchy(&path)
                        .map_err(|e| path.error(e.to_string()))
                }) {
                    Ok(hierarchy) => {
                        self.hierarchy = Some(hierarchy.into());
                    }
                    Err(err) => self.errors.push(err),
                };
            } else {
                attrs_buffer.push(attr)
            }
        }
        node.attrs = std::mem::take(&mut attrs_buffer);
        syn::visit_mut::visit_item_fn_mut(self, node)
    }

    fn visit_fn_arg_mut(&mut self, node: &mut FnArg) {
        let (name, node) = match (node.maybe_ident().cloned(), node) {
            (Some(name), FnArg::Typed(node)) => (name, node),
            _ => {
                return;
            }
        };
        let (field, mut errors) = maybe_parse_attribute_args_just_once::<LitStr>(node, "field");
        if let Some(field) = field {
            if let Some(files) = self.hierarchy.as_mut() {
                files
                    .args
                    .push(StructField::new(name.clone(), field.map(|l| l.value())));
            } else {
                self.errors.push(syn::Error::new(
                    name.span(),
                    format!("`field` attribute must be used on files test set"),
                ))
            }
        }
        self.errors.append(&mut errors);
        let (attr, mut errors) = attribute_args_once(node, "data");
        if let Some(attr) = attr {
            if let Some(files) = self.hierarchy.as_mut() {
                files.data.push(name.clone());
            } else {
                self.errors.push(syn::Error::new(
                    attr.span(),
                    format!("`data` attribute must be used on files test set"),
                ))
            }
        }
        self.errors.append(&mut errors);
    }
}

#[cfg(test)]
mod files_extractor {
    use super::*;

    mod should {
        use super::*;
        mod build_hierarchy {
            use super::*;

            use crate::{
                parse::sys::{mock::*, MockSysEngine},
                test::{assert_eq, *},
            };

            #[rstest]
            fn from_a_some_file_from_with_one_obj(#[from(sys_engine_lock)] _lock: SysEngineGuard) {
                let g_str = "my_path/**/*.json";
                let glob: LitStr = format!(r#""{g_str}""#).ast();
                let data = [
                    (
                        "/fake/root/my_path/a.json",
                        r#"{"age":42,"first_name":"Alice"}"#,
                    ),
                    (
                        "/fake/root/my_path/b.json",
                        r#"{"age":24,"first_name":"Bob"}"#,
                    ),
                    (
                        "/fake/root/my_path/other/some/path/o.json",
                        r#"{"age":99,"first_name":"Other"}"#,
                    ),
                ];
                let crate_root = PathBuf::from("/fake/root");

                let expected_hierarchy = Hierarchy {
                    folder: Folder::empty(&crate_root).add_folder(
                        Folder::empty("my_path")
                            .add_file(file::File {
                                name: std::ffi::OsStr::new("a.json").to_owned(),
                                content: JsonBody::root(r#"{"age":42,"first_name":"Alice"}"#),
                            })
                            .add_file(file::File {
                                name: std::ffi::OsStr::new("b.json").to_owned(),
                                content: JsonBody::root(r#"{"age":24,"first_name":"Bob"}"#),
                            })
                            .add_folder(Folder::empty("other").add_folder(
                                Folder::empty("some").add_folder(Folder::empty("path").add_file(
                                    file::File {
                                        name: std::ffi::OsStr::new("o.json").to_owned(),
                                        content: JsonBody::root(
                                            r#"{"age":99,"first_name":"Other"}"#,
                                        ),
                                    },
                                )),
                            )),
                    ),
                };

                let _ctx = Context::default()
                    .expected_crate_root(crate_root.clone())
                    .expected_glob(
                        g_str,
                        data.iter().map(|&(p, _d)| p.into()).collect::<Vec<_>>(),
                    )
                    .expected_file_context(&data);

                let extractor = FilesExtractor::<MockSysEngine>::default();
                let hierarchy = extractor.build_hierarchy(&glob).unwrap();

                assert_eq!(expected_hierarchy, hierarchy);
            }

            #[rstest]
            fn sorted(#[from(sys_engine_lock)] _lock: SysEngineGuard) {
                let g_str = "my_path/**/*.json";
                let glob: LitStr = format!(r#""{g_str}""#).ast();
                let data = [
                    ("/fake/root/my_path/a.json", "{}"),
                    ("/fake/root/my_path/e.json", "{}"),
                    ("/fake/root/my_path/d.json", "{}"),
                    ("/fake/root/my_path/c.json", "{}"),
                    ("/fake/root/my_path/b.json", "{}"),
                ];
                let crate_root = PathBuf::from("/fake/root");

                let empty_file = |name: &str| file::File {
                    name: std::ffi::OsStr::new(name).to_owned(),
                    content: JsonBody::root("{}"),
                };

                let expected_hierarchy = Hierarchy {
                    folder: Folder::empty(&crate_root).add_folder(
                        Folder::empty("my_path")
                            .add_file(empty_file("a.json"))
                            .add_file(empty_file("b.json"))
                            .add_file(empty_file("c.json"))
                            .add_file(empty_file("d.json"))
                            .add_file(empty_file("e.json")),
                    ),
                };

                let _ctx = Context::default()
                    .expected_crate_root(crate_root.clone())
                    .expected_glob(
                        g_str,
                        data.iter().map(|&(p, _d)| p.into()).collect::<Vec<_>>(),
                    )
                    .expected_file_context(&data);

                let extractor = FilesExtractor::<MockSysEngine>::default();
                let hierarchy = extractor.build_hierarchy(&glob).unwrap();

                assert_eq!(expected_hierarchy, hierarchy);
            }
        }
    }
}

#[cfg(test)]
mod parse_json_str_should {
    use super::*;
    use crate::test::{assert_eq, *};

    #[test]
    fn read_an_object_as_single_root_test() {
        let json_str = r#"{"age":42,"first_name":"Alice"}"#;
        assert_eq!(
            JsonBody::root(json_str),
            JsonBody::parse_json_str(json_str).unwrap()
        );
    }

    #[test]
    fn read_an_array_as_list_of_test_object() {
        let json_str = r#"[
                    {"age":42,"first_name":"Bob"},
                    {"age":24,"first_name":"Alice"}
                ]"#;
        assert_eq!(
            JsonBody::array()
                .add(r#"{"age":42,"first_name":"Bob"}"#)
                .add(r#"{"age":24,"first_name":"Alice"}"#),
            JsonBody::parse_json_str(json_str).unwrap()
        );
    }

    #[rstest]
    #[case("1.2")]
    #[case(r#""hello""#)]
    #[case(r#"null"#)]
    #[case::all_array_items_should_be_object(r#"[{},12]"#)]
    #[should_panic]
    fn raise_error(#[case] json_str: &str) {
        JsonBody::parse_json_str(json_str).unwrap();
    }
}
