use std::{
    self,
    process::{Command, Stdio},
    path::{PathBuf, Path},
    ffi::{OsStr, OsString},
    io::Write,
};

use toml_edit::{Document, Item, Array};
use std::fs::{File, read_to_string};
use std::io::Read;
use toml_edit::Table;
use std::borrow::Cow;
use std::sync::Arc;

#[derive(Clone)]
pub enum Channel {
    Stable,
    Beta,
    Nightly,
    Custom(String)
}

pub static CHANNEL_DEFAULT: Channel = Channel::Stable;
pub static ENV_CHANNEL: &'static str = "RSTEST_TEST_CHANNEL";

impl From<String> for Channel{
    fn from(value: String) -> Self {
        let s = value.to_string();
        match s.to_lowercase().as_str() {
            "stable" => Channel::Stable,
            "beta" => Channel::Beta,
            "nightly" => Channel::Nightly,
            _ => Channel::Custom(s),
        }
    }
}

impl Default for Channel {
    fn default() -> Self {
        std::env::var(ENV_CHANNEL)
            .ok()
            .map(Channel::from )
            .unwrap_or(CHANNEL_DEFAULT.clone())
    }
}

pub struct Project {
    pub name: OsString,
    root: PathBuf,
    channel: Channel,
    ws: Arc<std::sync::RwLock<()>>,
}

impl Project {
    const GLOBAL_TEST_ATTR: &'static str = "#![cfg(test)]";

    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_owned(),
            name: "project".into(),
            channel: Default::default(),
            ws: Arc::new(std::sync::RwLock::new(())),
        }.create()
    }

    pub fn get_name(&self) -> Cow<str> {
        self.name.to_string_lossy()
    }

    pub fn subproject<O: AsRef<OsStr>>(&self, name: O) -> Self {
        let _guard = self.ws.write().expect("Cannot lock workspace resource");
        self.workspace_add(name.as_ref().to_str().unwrap());
        Self {
            root: self.path(),
            name: name.as_ref().to_owned(),
            channel: self.channel.clone(),
            ws: self.ws.clone()
        }.create()
    }

    pub fn name<O: AsRef<OsStr>>(mut self, name: O) -> Self {
        self.name = name.as_ref().to_owned();
        self
    }

    pub fn path(&self) -> PathBuf {
        self.root.join(&self.name)
    }

    pub fn run_tests(&self) -> Result<std::process::Output, std::io::Error> {
        let _guard = self.ws.read().expect("Cannot lock workspace resource");
        if !self.has_test_global_attribute(self.code_path()) {
            self.add_test_global_attribute(self.code_path())
        }
        Command::new("cargo")
            .current_dir(&self.path())
            .arg(&self.cargo_channel_arg())
            .arg("test")
            .output()
    }

    pub fn compile(&self) -> Result<std::process::Output, std::io::Error> {
        let _guard = self.ws.read().expect("Cannot lock workspace resource");
        Command::new("cargo")
            .current_dir(&self.path())
            .arg("build")
            .output()
    }

    fn create(self) -> Self {
        match Command::new("cargo")
            .current_dir(&self.root)
            .arg("init")
            .arg(&self.name)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .code()
            .unwrap() {
            0 => {
                self.add_dependency();
                std::fs::File::create(self.code_path()).unwrap();
                self

            },

            code => panic!("cargo init return an error code: {}", code)
        }
    }

    fn has_test_global_attribute(&self, path: impl AsRef<Path>) -> bool{
        return read_to_string(&path).unwrap().starts_with(Self::GLOBAL_TEST_ATTR);
    }

    fn add_test_global_attribute(&self, path: impl AsRef<Path>) {
        let body = read_to_string(&path).unwrap();
        let mut out = std::fs::File::create(&path).unwrap();

        write!(out, "{}", Self::GLOBAL_TEST_ATTR).unwrap();
        write!(out, "{}", body).unwrap();
    }

    pub fn set_code_file<P: AsRef<Path>>(self, src: P) -> Self {
        std::fs::copy(
            src,
            self.code_path(),
        ).unwrap();
        self
    }

    pub fn append_code<S: AsRef<str>>(&self, code: S) {
        std::fs::OpenOptions::new()
            .append(true)
            .open(self.code_path())
            .unwrap()
            .write_all(code.as_ref().as_ref())
            .unwrap()
    }

    fn code_path(&self) -> PathBuf {
        self.path()
            .join("src")
            .join("lib.rs")
    }

    fn add_dependency(&self) {
        if 0 != Command::new("cargo")
            .current_dir(&self.path())
            .arg("add")
            .arg("rstest")
            .arg(&format!("--path={}",
                          std::env::current_dir().unwrap().as_os_str().to_str().unwrap()))
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .code()
            .unwrap() {
            panic!("cargo add return an error code");
        }
    }

    fn cargo_toml(&self) -> PathBuf {
        let mut path = self.path().clone();
        path.push("Cargo.toml");
        path
    }

    fn workspace_add(&self, prj: &str) {
        let mut orig = String::new();
        let path = &self.cargo_toml();
        File::open(path)
            .expect("cannot open Cargo.toml")
            .read_to_string(&mut orig)
            .expect("cannot read Cargo.toml");

        let mut doc = orig.parse::<Document>().expect("invalid Cargo.toml");

        let a : Array = Array::default();

        doc["workspace"].or_insert(Item::Table(Table::new()))
            ["members"].or_insert(Item::Value(a.into()))
            .as_array_mut().map(|a| a.push(prj));

        File::create(path)
            .expect("cannot update Cargo.toml")
            .write(doc.to_string().as_bytes())
            .expect("cannot write Cargo.toml");

    }

    fn cargo_channel_arg(&self) -> String {
        match &self.channel {
            Channel::Stable => "+stable".into(),
            Channel::Beta => "+beta".into(),
            Channel::Nightly => "+nightly".into(),
            Channel::Custom(name) => format!("+{}", name),
        }
    }
}
