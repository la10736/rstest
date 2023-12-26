use std::{ffi::OsStr, path::Path};

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct File<T> {
    pub(crate) name: std::ffi::OsString,
    pub(crate) content: T,
}

impl<T: Default> File<T> {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            content: Default::default(),
        }
    }

    pub(crate) fn content(mut self, content: T) -> Self {
        self.content = content;
        self
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

    fn add_file_in_sub_folder(&mut self, path: &[&OsStr], file: File<T>) {
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

    pub(crate) fn add_file_path(&mut self, path: &Path, test_file: File<T>) {
        let segments = path.iter().collect::<Vec<_>>();
        self.add_file_in_sub_folder(&segments, test_file);
    }
}
