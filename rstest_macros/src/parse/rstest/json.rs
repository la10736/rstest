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

use super::{attribute_args_once, maybe_parse_attribute_args_just_once};

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
    ParseError {
        path: PathBuf,
        source: file::ParseError,
    },
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
pub(crate) struct Files {
    hierarchy: Hierarchy,
    data: Vec<Ident>,
    args: Vec<StructField>,
}

impl Files {
    pub(crate) fn hierarchy(&self) -> &Hierarchy {
        &self.hierarchy
    }

    pub(crate) fn data(&self) -> &[Ident] {
        self.data.as_ref()
    }

    pub(crate) fn args(&self) -> &[StructField] {
        self.args.as_ref()
    }
}

impl quote::ToTokens for Files {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.data.iter().for_each(|data| data.to_tokens(tokens));
        self.args.iter().for_each(|f| f.to_tokens(tokens));
    }
}

impl From<Hierarchy> for Files {
    fn from(hierarchy: Hierarchy) -> Self {
        Self {
            hierarchy,
            data: Default::default(),
            args: Default::default(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Folder {
    pub(crate) name: std::ffi::OsString,
    pub(crate) files: Vec<file::Entry>,
    pub(crate) folders: Vec<Folder>,
}

impl Folder {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            files: Default::default(),
            folders: Default::default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn add_folder(mut self, folder: Folder) -> Self {
        self.folders.push(folder);
        self
    }

    #[cfg(test)]
    pub(crate) fn add_file(mut self, file: file::Entry) -> Self {
        self._add_file(file);
        self
    }

    fn _add_file(&mut self, file: file::Entry) {
        self.files.push(file);
    }

    fn add_file_in_sub_folder(&mut self, path: &[&OsStr], file: file::Entry) {
        if path.len() == 0 {
            self._add_file(file);
        } else {
            if let Some(inner) = self.folders.iter_mut().find(|f| &f.name == &path[0]) {
                inner.add_file_in_sub_folder(&path[1..], file)
            } else {
                let mut inner = Folder::empty(path[0]);
                inner.add_file_in_sub_folder(&path[1..], file);
                self.folders.push(inner);
            }
        }
    }

    pub(crate) fn add_file_path(&mut self, path: &Path, test_file: file::Entry) {
        let segments = path.iter().collect::<Vec<_>>();
        self.add_file_in_sub_folder(&segments, test_file);
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Hierarchy {
    pub(crate) folder: Folder,
}

impl<'a> From<&'a Path> for Hierarchy {
    fn from(abs: &'a Path) -> Self {
        Self {
            folder: Folder::empty(abs),
        }
    }
}

impl Hierarchy {
    fn add_file_relative(
        &mut self,
        path: &std::path::Path,
        test_file: file::Entry,
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
    pub(crate) hierarchy: Option<Files>,
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

impl<S: SysEngine> FilesExtractor<S> {
    fn build_hierarchy(&self, path: &LitStr) -> Result<Hierarchy, HierarchyError> {
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
                file::Bodies::parse_json_str(&data).map_err(|e| HierarchyError::ParseError {
                    path: path.to_owned(),
                    source: e,
                })?;
            let test_file = file::Entry::empty(fname).bodies(bodies);

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
                            .add_file(file::Entry {
                                name: std::ffi::OsStr::new("a.json").to_owned(),
                                tests: file::Bodies::root(r#"{"age":42,"first_name":"Alice"}"#),
                            })
                            .add_file(file::Entry {
                                name: std::ffi::OsStr::new("b.json").to_owned(),
                                tests: file::Bodies::root(r#"{"age":24,"first_name":"Bob"}"#),
                            })
                            .add_folder(Folder::empty("other").add_folder(
                                Folder::empty("some").add_folder(Folder::empty("path").add_file(
                                    file::Entry {
                                        name: std::ffi::OsStr::new("o.json").to_owned(),
                                        tests: file::Bodies::root(
                                            r#"{"age":99,"first_name":"Other"}"#,
                                        ),
                                    },
                                )),
                            )),
                    ),
                };

                let _cr = expected_crate_root(crate_root.clone());
                let _g = expected_glob(
                    g_str,
                    data.iter().map(|&(p, _d)| p.into()).collect::<Vec<_>>(),
                );
                let _rc = expected_file_context(&data);

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

                let empty_file = |name: &str| file::Entry {
                    name: std::ffi::OsStr::new(name).to_owned(),
                    tests: file::Bodies::root("{}"),
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

                let _cr = expected_crate_root(crate_root.clone());
                let _g = expected_glob(
                    g_str,
                    data.iter().map(|&(p, _d)| p.into()).collect::<Vec<_>>(),
                );
                let _rc = expected_file_context(&data);

                let extractor = FilesExtractor::<MockSysEngine>::default();
                let hierarchy = extractor.build_hierarchy(&glob).unwrap();

                assert_eq!(expected_hierarchy, hierarchy);
            }
        }
    }
}
