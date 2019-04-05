use std::fmt::Debug;

pub trait TearDown {
    fn tear_down(&self);
}

#[derive(Default)]
pub struct EmptyGuard {}

impl TearDown for EmptyGuard {
    fn tear_down(&self) {}
}

impl<A: TearDown, B: TearDown> TearDown for (A, B) {
    fn tear_down(&self) {
        self.0.tear_down();
        self.1.tear_down();
    }
}

pub struct Fixture<T, G: TearDown> {
    inner: Option<T>,
    guard: Option<G>,
}

impl<T, G: TearDown> Fixture<T, G> {
    fn new(inner: T, guard: G) -> Self {
        Fixture { inner: Some(inner), guard: Some(guard) }
    }

    pub fn take(&mut self) -> T {
        self.inner.take().unwrap()
    }

    pub fn guard(&mut self) -> G {
        self.guard.take().unwrap()
    }

    pub fn compose<OTHER: TearDown>(mut self, guard: OTHER) -> Fixture<T, (G, OTHER)> {
        Fixture::new(self.take(), (self.guard(), guard))
    }
}

impl<T: Debug, G: TearDown> Debug for Fixture<T, G> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Fixture<{:?}>", self.inner)
    }
}

impl<T, G: TearDown> Drop for Fixture<T, G> {
    fn drop(&mut self) {
        self.guard.take().map(|g| g.tear_down());
    }
}

impl<T> From<T> for Fixture<T, EmptyGuard> {
    fn from(inner: T) -> Self {
        Fixture::new(inner, Default::default())
    }
}
