use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use syn::{spanned::Spanned, visit_mut::VisitMut, FnArg, Ident, ItemFn, LitStr};
use thiserror::Error;

use crate::{error::RaiseError, parse::sys::SysEngine, refident::MaybeIdent, utils::attr_is};

use super::{attribute_args_once, maybe_parse_attribute_args_just_once};

#[derive(Error, Debug)]
pub(crate) enum HierarchyError {
    #[error("Not a file error: {0}")]
    NotAFile(PathBuf),
    #[error("The file should be in a folder: '{0}'")]
    NotInFolder(PathBuf),
    #[error("Cannot read file '{path}' due: {source}")]
    ReadFileError {
        path: PathBuf,
        source: std::io::Error,
    },
    #[error("Invalid json '{path}' due: {source}")]
    InvalidJson {
        path: PathBuf,
        source: json::JsonError,
    },
    #[error("Should be a object or an array of object: '{0}'")]
    InvalidJsonData(PathBuf),
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
pub(crate) enum TestFileBodies {
    Root(String),
    Array(Vec<String>),
}

impl Default for TestFileBodies {
    fn default() -> Self {
        Self::Array(Vec::default())
    }
}

impl TestFileBodies {
    pub(crate) fn root(body: &str) -> Self {
        Self::Root(body.to_string())
    }

    pub(crate) fn array() -> Self {
        Self::Array(Default::default())
    }

    pub(crate) fn add(self, body: &str) -> Self {
        match self {
            TestFileBodies::Root(b) => TestFileBodies::Array(vec![b, body.to_string()]),
            TestFileBodies::Array(mut bodies) => {
                bodies.push(body.to_string());
                TestFileBodies::Array(bodies)
            }
        }
    }
}
#[derive(PartialEq, Debug, Clone)]
pub(crate) struct TestFile {
    pub(crate) name: std::ffi::OsString,
    pub(crate) tests: TestFileBodies,
}

impl TestFile {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            tests: Default::default(),
        }
    }
    pub(crate) fn bodies(mut self, bodes: TestFileBodies) -> Self {
        self.tests = bodes;
        self
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Folder {
    pub(crate) name: std::ffi::OsString,
    pub(crate) files: Vec<TestFile>,
    pub(crate) folders: Vec<Folder>,
}

enum FolderChild {
    File(TestFile),
    Folder(Folder),
}

impl Folder {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            files: Default::default(),
            folders: Default::default(),
        }
    }

    fn add(&mut self, child: FolderChild) {
        match child {
            FolderChild::File(file) => {
                self.files.push(file);
            }
            FolderChild::Folder(folder) => {
                self.folders.push(folder);
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn add_folder(mut self, folder: Folder) -> Self {
        self.add(FolderChild::Folder(folder));
        self
    }

    #[cfg(test)]
    pub(crate) fn add_file(mut self, file: TestFile) -> Self {
        self._add_file(file);
        self
    }

    fn _add_file(&mut self, file: TestFile) {
        self.add(FolderChild::File(file));
    }

    fn add_file_in_sub_folder(&mut self, path: &[&OsStr], file: TestFile) {
        if path.len() == 0 {
            self._add_file(file);
        } else {
            if let Some(inner) = self.folders.iter_mut().find(|f| f.name == self.name) {
                inner.add_file_in_sub_folder(&path[1..], file)
            } else {
                let mut inner = Folder::empty(path[0]);
                inner.add_file_in_sub_folder(&path[1..], file);
                self.folders.push(inner);
            }
        }
    }

    pub(crate) fn add_file_path(&mut self, path: &Path, test_file: TestFile) {
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
        test_file: TestFile,
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
    fn build_hierarchy(&self, path: LitStr) -> syn::Result<Hierarchy> {
        let crate_root = S::crate_root().map_err(|m| path.error(m))?;
        let mut paths = S::glob(&path.value())
            .map_err(|e| path.error(format!("Cannot get files from glob due: {e}")))?;
        paths.sort();
        let mut hierarchy = Hierarchy::from(crate_root.as_path());
        for path in paths {
            let fname = path
                .file_name()
                .ok_or_else(|| HierarchyError::NotAFile(path.to_path_buf()))
                .unwrap();
            let data = S::read_file(&path.to_string_lossy()).unwrap();
            let test_file = match json::parse(&data)
                .map_err(|e| HierarchyError::InvalidJson {
                    path: path.to_owned(),
                    source: e,
                })
                .unwrap()
            {
                json::JsonValue::Object(_) => Ok(TestFile::empty(fname)),
                json::JsonValue::Array(array) => Ok(TestFile::empty(fname).bodies({
                    let mut bodies = TestFileBodies::array();
                    for a in array {
                        bodies = bodies.add(&json::stringify(a));
                    }
                    bodies
                })),
                _ => Err(HierarchyError::InvalidJsonData(path.to_path_buf())),
            }
            .unwrap();

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
                match attr
                    .parse_args::<LitStr>()
                    .and_then(|path| self.build_hierarchy(path))
                {
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
