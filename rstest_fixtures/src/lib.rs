use std::fmt::Debug;

pub trait TearDown {
    fn tear_down(self);
}

#[derive(Default)]
pub struct EmptyGuard {}

impl TearDown for EmptyGuard {
    fn tear_down(self) {}
}

impl<A: TearDown, B: TearDown> TearDown for (A, B) {
    fn tear_down(self) {
        self.0.tear_down();
        self.1.tear_down();
    }
}

pub struct Fixture<T, G: TearDown> {
    inner: Option<T>,
    guard: Option<G>,
}

impl<T, G: TearDown> Fixture<T, G> {
    pub fn new(inner: T, guard: G) -> Self {
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

pub struct TearDownClosure<F:FnOnce() -> ()>(F);

impl<F: FnOnce() -> ()> TearDown for TearDownClosure<F> {
    fn tear_down(self) {
        self.0()
    }
}

impl<F: FnOnce()->()> From<F> for TearDownClosure<F> {
    fn from(closure: F) -> Self {
        TearDownClosure(closure)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn fixture_should_wrap_value_and_run_guard_when_destroyed() {
        let mut guard = RefCell::new(vec![]);
        {
            let _fixture = Fixture::new(42, TearDownClosure(
                || guard.get_mut().push("destroyed"))
            );
            // _fixture will be dropped here and tear_down() executed
        }

        assert_eq!(*guard.borrow(), vec!["destroyed"]);
    }

    #[test]
    fn tear_down_closure_should_can_move_all_needed_data() {
        let guard = Rc::new(RefCell::new(vec![]));
        {
            let inner_guard = guard.clone();
            let _fixture = Fixture::new("some", TearDownClosure(
               move || inner_guard.borrow_mut().push("destroyed"))
            );
            // _fixture will be dropped here and tear_down() executed
        }

        assert_eq!(*guard.borrow(), vec!["destroyed"]);
    }



    #[test]
    fn fixture_composition_use_case() {
        // Take a fixture `f` that use two other fixtures `f1`, `f2` and an it's own tear_down `t`
        // => destroy f should call first `t.tear_down()` then `f2.tear_down()` and finally `f1.tear_down()`
        // In other words should finally its self and then its arguments in stack reverse order.

        let guard = RefCell::new(vec![]);
        let mut f1 = Fixture::new((), TearDownClosure(
            || guard.borrow_mut().push("destroyed_f1"))
        );
        let mut f2 = Fixture::new((), TearDownClosure(
            || guard.borrow_mut().push("destroyed_f2"))
        );

        {
            let _f = Fixture::new((),
                                 TearDownClosure(|| guard.borrow_mut().push("destroyed_f")))
                .compose(f2.guard())
                .compose(f1.guard());
        }

        assert_eq!(*guard.borrow(), vec!["destroyed_f","destroyed_f2","destroyed_f1"]);
    }

    #[test]
    fn after_moved_the_guard_fixture_can_be_released_without_invoke_tear_down() {
        let guard = RefCell::new(vec![]);
        let mut f = Fixture::new((), TearDownClosure(
            || guard.borrow_mut().push("destroyed_f1"))
        );
        {
            let _inner_guard = f.guard();
            {
                // Here we move the fixture to release it
                let _local_f = f;
            }
            assert_eq!(*guard.borrow(), Vec::<&str>::new());
            // Here we drop inner guard but again this dosn't invoke tear_down() trait
        }
        assert_eq!(*guard.borrow(), Vec::<&str>::new());
    }
}
