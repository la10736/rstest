use std::{
    self,
    ffi::{OsStr, OsString},
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use std::borrow::Cow;
use std::fs::{read_to_string, File};
use std::io::Read;
use std::sync::Arc;
use toml_edit::{Array, Document, Item, Table};

#[derive(Clone)]
pub enum Channel {
    Stable,
    Beta,
    Nightly,
    Custom(String),
}

pub static CHANNEL_DEFAULT: Channel = Channel::Stable;
pub static ENV_CHANNEL: &str = "RSTEST_TEST_CHANNEL";

impl From<String> for Channel {
    fn from(value: String) -> Self {
        match value.to_lowercase().as_str() {
            "stable" => Channel::Stable,
            "beta" => Channel::Beta,
            "nightly" => Channel::Nightly,
            _ => Channel::Custom(value),
        }
    }
}

impl Default for Channel {
    fn default() -> Self {
        std::env::var(ENV_CHANNEL)
            .ok()
            .map(Channel::from)
            .unwrap_or_else(|| CHANNEL_DEFAULT.clone())
    }
}

pub struct Project {
    pub name: OsString,
    root: PathBuf,
    channel: Channel,
    nocapture: bool,
    ws: Arc<std::sync::RwLock<()>>,
}

impl Project {
    const GLOBAL_TEST_ATTR: &'static str = "#![cfg(test)]";

    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_owned(),
            name: "project".into(),
            channel: Default::default(),
            nocapture: false,
            ws: Arc::new(std::sync::RwLock::new(())),
        }
        .create()
    }

    pub fn get_name(&self) -> Cow<str> {
        self.name.to_string_lossy()
    }

    pub fn with_nocapture(mut self) -> Self {
        self.nocapture = true;
        self
    }

    pub fn subproject<O: AsRef<OsStr>>(&self, name: O) -> Self {
        let _guard = self.ws.write().expect("Cannot lock workspace resource");
        self.workspace_add(name.as_ref().to_str().unwrap());
        Self {
            root: self.path(),
            name: name.as_ref().to_owned(),
            channel: self.channel.clone(),
            nocapture: self.nocapture,
            ws: self.ws.clone(),
        }
        .create()
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
        let mut cmd = Command::new("cargo");

        cmd.current_dir(&self.path())
            .arg(&self.cargo_channel_arg())
            .arg("test");

        if self.nocapture {
            cmd.args(["--", "--nocapture"]);
        }

        cmd.output()
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
            .args(vec!["--edition", "2018"])
            .arg(&self.name)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap()
            .wait()
            .unwrap()
            .code()
            .unwrap()
        {
            0 => {
                std::fs::File::create(self.code_path()).unwrap();
                self
            }

            code => panic!("cargo init return an error code: {}", code),
        }
    }

    fn has_test_global_attribute(&self, path: impl AsRef<Path>) -> bool {
        read_to_string(&path)
            .unwrap()
            .starts_with(Self::GLOBAL_TEST_ATTR)
    }

    fn add_test_global_attribute(&self, path: impl AsRef<Path>) {
        let body = read_to_string(&path).unwrap();
        let mut out = std::fs::File::create(&path).unwrap();

        write!(out, "{}", Self::GLOBAL_TEST_ATTR).unwrap();
        write!(out, "{}", body).unwrap();
    }

    pub fn set_code_file<P: AsRef<Path>>(self, src: P) -> Self {
        std::fs::copy(src, self.code_path()).unwrap();
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

    pub fn add_dependency(&self, crate_name: &str, attrs: &str) {
        let mut doc = self.read_cargo_toml();

        doc["dependencies"].or_insert(Item::Table(Table::new()))[crate_name]
            .or_insert(Item::Value(attrs.parse().unwrap()));

        self.save_cargo_toml(&doc);
    }

    pub fn add_local_dependency(&self, name: &str) {
        self.add_dependency(
            name,
            format!(r#"{{path="{}"}}"#, self.exec_dir_str()).as_str(),
        );
    }

    pub fn exec_dir_str(&self) -> String {
        std::env::current_dir()
            .unwrap()
            .as_os_str()
            .to_str()
            .unwrap()
            .to_owned()
    }

    fn workspace_add(&self, prj: &str) {
        let mut doc = self.read_cargo_toml();

        let members: Array = Array::default();

        if let Some(members) = doc["workspace"].or_insert(Item::Table(Table::new()))["members"]
            .or_insert(Item::Value(members.into()))
            .as_array_mut()
        {
            members.push(prj)
        }

        self.save_cargo_toml(&doc);
    }

    fn code_path(&self) -> PathBuf {
        self.path().join("src").join("lib.rs")
    }

    fn cargo_toml_path(&self) -> PathBuf {
        let mut path = self.path();
        path.push("Cargo.toml");
        path
    }

    fn read_cargo_toml(&self) -> Document {
        let mut orig = String::new();
        File::open(self.cargo_toml_path())
            .expect("cannot open Cargo.toml")
            .read_to_string(&mut orig)
            .expect("cannot read Cargo.toml");

        orig.parse::<Document>().expect("invalid Cargo.toml")
    }

    fn save_cargo_toml(&self, doc: &Document) {
        File::create(self.cargo_toml_path())
            .expect("cannot update Cargo.toml")
            .write_all(doc.to_string().as_bytes())
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
