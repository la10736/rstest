use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use relative_path::{Component, RelativePath};
use thiserror::Error;

use crate::parse::sys::SysEngine;

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct File<T> {
    pub(crate) name: std::ffi::OsString,
    pub(crate) content: T,
}

impl<T> File<T> {
    pub(crate) fn new(name: std::ffi::OsString, content: T) -> Self {
        Self { name, content }
    }

    pub(crate) fn content(mut self, content: T) -> Self {
        self.content = content;
        self
    }
}

impl<T: Default> File<T> {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            content: Default::default(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Folder<T> {
    pub(crate) name: std::ffi::OsString,
    pub(crate) files: Vec<File<T>>,
    pub(crate) folders: Vec<Folder<T>>,
}

impl<T> Folder<T> {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            files: vec![],
            folders: vec![],
        }
    }

    #[cfg(test)]
    pub(crate) fn add_folder(mut self, folder: Folder<T>) -> Self {
        self.folders.push(folder);
        self
    }

    #[cfg(test)]
    pub(crate) fn add_file(mut self, file: File<T>) -> Self {
        self._add_file(file);
        self
    }

    fn _add_file(&mut self, file: File<T>) {
        self.files.push(file);
    }

    fn add_file_in_sub_folder(&mut self, path: &[&str], file: File<T>) {
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

    pub(crate) fn add_file_path(&mut self, path: &RelativePath, test_file: File<T>) {
        let segments = path.iter().collect::<Vec<_>>();
        self.add_file_in_sub_folder(&segments, test_file);
    }
}

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
        source: super::ParseError,
    },
    #[error("Invalid Absolute path: '{s}'")]
    AbsPath { s: String },
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
        path: &RelativePath,
        test_file: File<T>,
    ) -> Result<(), HierarchyError> {
        let parent = path
            .parent()
            .ok_or_else(|| HierarchyError::NotInFolder(path.to_logical_path(".")))?;

        self.folder.add_file_path(parent, test_file);
        Ok(())
    }

    pub fn build<S: SysEngine, F>(
        crate_root: &Path,
        files: Vec<F>,
        get_abs_path: impl Fn(&F) -> Result<PathBuf, HierarchyError>,
        mut get_content: impl FnMut(&F, &PathBuf, &RelativePath) -> Result<T, HierarchyError>,
    ) -> Result<Self, HierarchyError> {
        let mut hierarchy = Hierarchy::from(crate_root);
        for f in files {
            let abs_path = get_abs_path(&f)?;
            let relative_path = abs_path
                .clone()
                .into_os_string()
                .into_string()
                .map(|inner| {
                    RelativePath::new(&crate_root.as_os_str().to_string_lossy()).relative(inner)
                })
                .map_err(|e| HierarchyError::AbsPath {
                    s: e.to_string_lossy().to_string(),
                })?;

            let fname = abs_path
                .file_name()
                .ok_or_else(|| HierarchyError::NotAFile(abs_path.clone()))?;
            let test_file = File::new(
                fname.to_owned(),
                get_content(&f, &abs_path, &relative_path)?,
            );

            hierarchy.add_file_relative(&relative_path, test_file)?;
        }
        Ok(hierarchy)
    }
}
