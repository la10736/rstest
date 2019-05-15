use rstest::*;
use lazy_static::lazy_static;
use std::path::{PathBuf, Path};
use std::str::FromStr;

#[derive(PartialEq, Debug, Default)]
struct Data {}
impl FromStr for Data {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Ok(Default::default())
        } else {
            Err("Not Implemented Yet".to_owned())
        }
    }
}

type SyncGuard = std::sync::MutexGuard<'static, ()>;
#[fixture]
fn synchronize() -> SyncGuard {
    use std::sync::Mutex;
    lazy_static!{
        static ref MUTEX : Mutex<()> = Mutex::new(());
    }
    MUTEX.lock().unwrap()
}

struct TempFile(PathBuf);
impl AsRef<Path> for TempFile {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}
impl Drop for TempFile {
    fn drop(&mut self) {
        std::fs::remove_file(&self.0).unwrap();
    }
}

#[fixture]
#[allow(unused_variables)]
fn empty(synchronize: SyncGuard) -> TempFile {
    use std::fs::File;
    for n in 0.. {
        let path = format!("empty_{}", n).into();
        if File::open(&path).is_err() {
            File::create(&path).expect("Cannot open");
            return TempFile(path)
        }
    }
    unreachable!()
}

#[rstest]
fn should_parse_empty_file<P: AsRef<Path>>(empty: P) {
    let content = std::fs::read_to_string(&empty).unwrap();

    let data: Data = content.parse().unwrap();

    assert_eq!(Data::default(), data);
}

#[rstest]
fn should_parse_empty_file2<P: AsRef<Path>>(empty: P) {
    let content = std::fs::read_to_string(&empty).unwrap();

    let data: Data = content.parse().unwrap();

    assert_eq!(Data::default(), data);
}
