use std::path::{Path, PathBuf};

use relative_path::RelativePath;
use thiserror::Error;

use crate::parse::sys::SysEngine;

#[derive(PartialEq, Debug, Clone)]
pub struct File<T> {
    pub(crate) name: std::ffi::OsString,
    pub(crate) content: T,
}

impl<T> File<T> {
    pub(crate) fn new<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S, content: T) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            content,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Folder<T> {
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

    pub fn add_file_path(&mut self, path: &RelativePath, test_file: File<T>) {
        let segments = path.iter().collect::<Vec<_>>();
        self.add_file_in_sub_folder(&segments, test_file);
    }

    #[allow(dead_code)]
    pub fn iter(&self) -> impl Iterator<Item = Entry<T>> {
        FolderIterator::init(self)
    }

    fn no_files(&self) -> bool {
        self.files.is_empty() && self.folders.iter().all(|f| f.no_files())
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
    pub fn no_files(&self) -> bool {
        self.folder.no_files()
    }

    pub fn build<'a, S: SysEngine, WRAP>(
        crate_root: &Path,
        wraps: &'a [WRAP],
        get_path: impl Fn(&WRAP) -> &Path,
        mut get_content: impl FnMut(&Path, &RelativePath) -> Result<T, HierarchyError>,
    ) -> Result<Self, (&'a WRAP, HierarchyError)> {
        let mut hierarchy = Hierarchy::from(crate_root);
        for f in wraps {
            hierarchy
                .process_path::<S>(crate_root, get_path(f), &mut get_content)
                .map_err(|e| (f, e))?
        }
        Ok(hierarchy)
    }

    fn process_path<'a, S: SysEngine>(
        &mut self,
        crate_root: &Path,
        abs_path: &'a Path,
        mut get_content: impl FnMut(&Path, &RelativePath) -> Result<T, HierarchyError>,
    ) -> Result<(), HierarchyError> {
        let relative_path = abs_path
            .as_os_str()
            .to_str()
            .map(|inner| {
                RelativePath::new(&crate_root.as_os_str().to_string_lossy()).relative(inner)
            })
            .ok_or_else(|| HierarchyError::AbsPath {
                s: abs_path.to_string_lossy().to_string(),
            })?;

        let fname = abs_path
            .file_name()
            .ok_or_else(|| HierarchyError::NotAFile(abs_path.to_owned()))?;
        let test_file = File::new(fname, get_content(abs_path, &relative_path)?);

        let parent = relative_path
            .parent()
            .ok_or_else(|| HierarchyError::NotInFolder(relative_path.to_logical_path(".")))?;
        self.folder.add_file_path(&parent, test_file);
        Ok(())
    }
}

pub enum Entry<'a, T> {
    File(&'a File<T>),
    Folder(&'a Folder<T>),
}

impl<T> IntoIterator for Hierarchy<T> {
    type Item = File<T>;

    type IntoIter = FolderFileIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.folder.into_iter()
    }
}

impl<T> IntoIterator for Folder<T> {
    type Item = File<T>;

    type IntoIter = FolderFileIterator<T>;

    fn into_iter(self) -> Self::IntoIter {
        FolderFileIterator {
            files: self.files.into_iter(),
            folders: self.folders.into_iter(),
            sub_iterator: None,
            file: None,
            folder: None,
        }
    }
}

pub struct FolderFileIterator<T> {
    files: std::vec::IntoIter<File<T>>,
    folders: std::vec::IntoIter<Folder<T>>,
    sub_iterator: Option<Box<FolderFileIterator<T>>>,
    file: Option<File<T>>,
    folder: Option<Folder<T>>,
}

impl<T> Iterator for FolderFileIterator<T> {
    type Item = File<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(it) = self.sub_iterator.as_mut() {
            match it.next() {
                Some(v) => return Some(v),
                None => {
                    self.sub_iterator = None;
                }
            }
        }
        if self.file.is_none() {
            self.file = self.files.next();
        }
        if self.folder.is_none() {
            self.folder = self.folders.next();
        }
        match (&self.file, self.folder.take()) {
            (None, None) => None,
            (None, Some(folder)) => {
                self.sub_iterator = Some(Box::new(folder.into_iter()));
                self.next()
            }
            (Some(_), None) => self.file.take(),
            (Some(file), Some(folder)) => {
                if file.name < folder.name {
                    self.folder = Some(folder);
                    self.file.take()
                } else {
                    self.sub_iterator = Some(Box::new(folder.into_iter()));
                    self.next()
                }
            }
        }
    }
}

struct FolderIterator<'a, T> {
    this: Option<&'a Folder<T>>,
    files: std::slice::Iter<'a, File<T>>,
    folders: std::slice::Iter<'a, Folder<T>>,
    sub_iterator: Option<Box<FolderIterator<'a, T>>>,
    file: Option<&'a File<T>>,
    folder: Option<&'a Folder<T>>,
}

impl<'a, T> FolderIterator<'a, T> {
    fn init(folder: &'a Folder<T>) -> Self {
        Self {
            this: Some(folder),
            files: folder.files.iter(),
            folders: folder.folders.iter(),
            sub_iterator: None,
            file: None,
            folder: None,
        }
    }
}

impl<'a, T: 'a> Iterator for FolderIterator<'a, T> {
    type Item = Entry<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(this) = self.this.take() {
            return Some(Entry::Folder(this));
        }
        if let Some(it) = self.sub_iterator.as_mut() {
            match it.next() {
                Some(v) => return Some(v),
                None => {
                    self.sub_iterator = None;
                }
            }
        }
        if self.file.is_none() {
            self.file = self.files.next();
        }
        if self.folder.is_none() {
            self.folder = self.folders.next();
        }
        match (&self.file, &self.folder) {
            (None, None) => None,
            (None, Some(folder)) => {
                self.sub_iterator = Some(Box::new(FolderIterator::init(folder)));
                self.folder = None;
                self.next()
            }
            (Some(_), None) => self.file.take().map(Entry::File),
            (Some(file), Some(folder)) => {
                if file.name < folder.name {
                    self.file.take().map(Entry::File)
                } else {
                    self.sub_iterator = Some(Box::new(FolderIterator::init(folder)));
                    self.folder = None;
                    self.next()
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;

    use super::*;

    impl<'a, T> Entry<'a, T> {
        pub fn name(self) -> &'a OsStr {
            match self {
                Entry::File(f) => f.name.as_os_str(),
                Entry::Folder(f) => f.name.as_os_str(),
            }
        }
    }

    #[test]
    fn folder_iterator() {
        let root = Folder::<u32>::empty("root")
            .add_file(File::new("z", 3))
            .add_file(File::new("a", 0))
            .add_file(File::new("b", 1))
            .add_folder(Folder::empty("sub").add_file(File::new("a", 2)));

        let names: Vec<_> = root
            .iter()
            .map(|e| e.name().to_string_lossy().to_string())
            .collect();
        let index: Vec<_> = root
            .iter()
            .filter_map(|f| match f {
                Entry::File(File { content, .. }) => Some(*content),
                _ => None,
            })
            .collect();

        assert_eq!(names, vec!["root", "sub", "a", "z", "a", "b"]);
        assert_eq!(index, vec![2, 3, 0, 1]);
    }

    #[test]
    fn folder_into_iterator() {
        let root = Folder::<u32>::empty("root")
            .add_file(File::new("z", 3))
            .add_file(File::new("a", 0))
            .add_file(File::new("b", 1))
            .add_folder(Folder::empty("sub").add_file(File::new("a", 2)));

        let index: Vec<_> = root.into_iter().map(|f| f.content).collect();

        assert_eq!(index, vec![2, 3, 0, 1]);
    }

    #[test]
    fn no_files_should_return_true_if_no_files() {
        assert!(Hierarchy::<()>::from(Path::new("some")).no_files());
        assert!(Folder::<()>::empty("some").no_files());
        assert!(Folder::<()>::empty("root")
            .add_folder(Folder::empty("sub"))
            .add_folder(Folder::empty("sub_2"))
            .no_files());
    }

    #[test]
    fn no_files_should_return_false_if_some_files() {
        assert!(!Folder::<()>::empty("some")
            .add_file(File::empty("a"))
            .no_files());
        assert!(!Folder::<()>::empty("root")
            .add_folder(Folder::empty("sub"))
            .add_file(File::empty("a"))
            .add_folder(Folder::empty("sub_2"))
            .no_files());
        assert!(!Folder::<()>::empty("root")
            .add_folder(Folder::empty("sub"))
            .add_folder(Folder::empty("sub_2"))
            .add_file(File::empty("a"))
            .no_files());
    }
}
