use std::{
    self,
    process::{Command, Stdio},
    path::{PathBuf, Path},
    ffi::{OsStr, OsString},
    io::Write,
};

use toml_edit::{Document, Item, Array};
use std::fs::File;
use std::io::Read;
use toml_edit::Table;
use std::borrow::Cow;

pub enum Channel {
    Stable,
    Beta,
    Nigtly,
    Custom(String)
}

pub struct Project {
    pub name: OsString,
    root: PathBuf,
    channel: Channel,
    ws: std::sync::Mutex<()>,
}

impl Project {
    pub fn new<P: AsRef<Path>>(root: P) -> Self {
        Self {
            root: root.as_ref().to_owned(),
            name: "project".into(),
            channel: Channel::Stable,
            ws: std::sync::Mutex::new(()),
        }
    }

    pub fn get_name(&self) -> Cow<str> {
        self.name.to_string_lossy()
    }

    pub fn name<O: AsRef<OsStr>>(mut self, name: O) -> Self {
        self.name = name.as_ref().to_owned();
        self
    }

    pub fn path(&self) -> PathBuf {
        self.root.join(&self.name)
    }

    pub fn run_tests(&self) -> Result<std::process::Output, std::io::Error> {
        Command::new("cargo")
            .current_dir(&self.path())
            .arg(&self.cargo_channel_arg())
            .arg("test")
            .output()
    }

    pub fn compile(&self) -> Result<std::process::Output, std::io::Error> {
        Command::new("cargo")
            .current_dir(&self.path())
            .arg("build")
            .output()
    }

    pub fn create(self) -> Self {
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

    pub fn workspace_add(&self, prj: &str) {
        let _guard = self.ws.lock().expect("Cannot lock workspace resource");
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
            Channel::Nigtly => "+nigtly".into(),
            Channel::Custom(name) => format!("+{}", name),
        }
    }
}
