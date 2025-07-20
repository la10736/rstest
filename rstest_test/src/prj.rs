use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

use rand::Rng;
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

impl Display for Channel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Channel::Stable => write!(f, "+stable"),
            Channel::Beta => write!(f, "+beta"),
            Channel::Nightly => write!(f, "+nightly"),
            Channel::Custom(name) => write!(f, "+{name}"),
        }
    }
}

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

fn read_channel_env() -> Option<Channel> {
    std::env::var(ENV_CHANNEL).ok().map(Channel::from)
}

pub struct Project {
    pub name: OsString,
    root: PathBuf,
    channel: Option<Channel>,
    nocapture: bool,
    ws: Arc<std::sync::RwLock<()>>,
    default_timeout: Option<u64>,
}

impl Project {
    const GLOBAL_TEST_ATTR: &'static str = "#![cfg(test)]";

    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_owned(),
            name: "project".into(),
            channel: read_channel_env(),
            nocapture: false,
            ws: Arc::new(std::sync::RwLock::new(())),
            default_timeout: Default::default(),
        }
        .create()
    }

    pub fn get_name(&self) -> Cow<'_, str> {
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
            default_timeout: Default::default(),
        }
        .create()
    }

    pub fn name<O: AsRef<OsStr>>(mut self, name: O) -> Self {
        name.as_ref().clone_into(&mut self.name);
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
        let mut cmd = self.cargo_cmd();

        if let Some(timeout) = self.default_timeout {
            cmd.env("RSTEST_TIMEOUT", timeout.to_string());
        }

        cmd.arg("test");

        if self.nocapture {
            cmd.args(["--", "--nocapture"]);
        }

        cmd.output()
    }

    pub fn compile(&self) -> Result<std::process::Output, std::io::Error> {
        let _guard = self.ws.read().expect("Cannot lock workspace resource");
        self.cargo_cmd().arg("build").arg("--tests").output()
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
        write!(out, "{body}").unwrap();
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

    pub fn add_path_dependency(&self, name: &str, path: &str) {
        let path = crate::to_toml_string(path.to_string());
        self.add_dependency(name, format!(r#"{{path={path}}}"#).as_str());
    }

    pub fn add_local_dependency(&self, name: &str) {
        self.add_path_dependency(name, &self.exec_dir_str());
    }

    pub fn exec_dir_str(&self) -> String {
        std::env::current_dir()
            .unwrap()
            .as_os_str()
            .to_str()
            .unwrap()
            .to_owned()
    }

    fn cargo_cmd(&self) -> Command {
        let mut cmd = Command::new("cargo");
        if let Some(channel) = &self.cargo_channel_arg() {
            cmd.arg(channel);
        }
        cmd.current_dir(self.path());
        cmd
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
        // Avoid to have concurrent access to cargo workspace manifest:
        // leverage on atomic file operations to avoid race conditions.
        let path = self.cargo_toml_path();
        let mut temp_path = path.clone();
        let rand_name_part: u64 = rand::rng().random();
        temp_path.set_file_name(format!("Cargo_{rand_name_part:021}.toml"));
        File::create(&temp_path)
            .unwrap_or_else(|_| panic!("cannot create {}", temp_path.display()))
            .write_all(doc.to_string().as_bytes())
            .unwrap_or_else(|_| panic!("cannot write {}", temp_path.display()));
        std::fs::rename(&temp_path, path)
            .unwrap_or_else(|_| panic!("cannot rename {} -> Cargo.toml", temp_path.display()));
    }

    fn cargo_channel_arg(&self) -> Option<String> {
        self.channel.as_ref().map(ToString::to_string)
    }

    // in seconds
    pub fn set_default_timeout(&mut self, timeout: u64) {
        self.default_timeout = Some(timeout);
    }
}
