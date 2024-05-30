use std::collections::HashMap;

use syn::Ident;

#[derive(PartialEq, Debug, Clone, Copy)]
#[allow(dead_code)]
#[derive(Default)]
pub(crate) enum FutureArg {
    #[default]
    None,
    Define,
    Await,
}

#[derive(PartialEq, Default, Debug)]
pub(crate) struct ArgumentInfo {
    future: FutureArg,
    by_ref: bool,
}

impl ArgumentInfo {
    fn future(future: FutureArg) -> Self {
        Self {
            future,
            ..Default::default()
        }
    }

    fn by_ref() -> Self {
        Self {
            by_ref: true,
            ..Default::default()
        }
    }

    fn is_future(&self) -> bool {
        use FutureArg::*;

        matches!(self.future, Define | Await)
    }

    fn is_future_await(&self) -> bool {
        use FutureArg::*;

        matches!(self.future, Await)
    }

    fn is_by_ref(&self) -> bool {
        self.by_ref
    }
}

#[derive(PartialEq, Default, Debug)]
pub(crate) struct ArgumentsInfo {
    args: HashMap<Ident, ArgumentInfo>,
    is_global_await: bool,
    once: Option<syn::Attribute>,
}

impl ArgumentsInfo {
    pub(crate) fn set_future(&mut self, ident: Ident, kind: FutureArg) {
        self.args
            .entry(ident)
            .and_modify(|v| v.future = kind)
            .or_insert_with(|| ArgumentInfo::future(kind));
    }

    pub(crate) fn set_futures(&mut self, futures: impl Iterator<Item = (Ident, FutureArg)>) {
        futures.for_each(|(ident, k)| self.set_future(ident, k));
    }

    pub(crate) fn set_global_await(&mut self, is_global_await: bool) {
        self.is_global_await = is_global_await;
    }

    #[allow(dead_code)]
    pub(crate) fn add_future(&mut self, ident: Ident) {
        self.set_future(ident, FutureArg::Define);
    }

    pub(crate) fn is_future(&self, id: &Ident) -> bool {
        self.args
            .get(id)
            .map(|arg| arg.is_future())
            .unwrap_or_default()
    }

    pub(crate) fn is_future_await(&self, ident: &Ident) -> bool {
        match self.args.get(ident) {
            Some(arg) => arg.is_future_await() || (arg.is_future() && self.is_global_await()),
            None => false,
        }
    }

    pub(crate) fn is_global_await(&self) -> bool {
        self.is_global_await
    }

    pub(crate) fn set_once(&mut self, once: Option<syn::Attribute>) {
        self.once = once
    }

    pub(crate) fn get_once(&self) -> Option<&syn::Attribute> {
        self.once.as_ref()
    }

    pub(crate) fn is_once(&self) -> bool {
        self.get_once().is_some()
    }

    pub(crate) fn set_by_ref(&mut self, ident: Ident) {
        self.args
            .entry(ident)
            .and_modify(|v| v.by_ref = true)
            .or_insert_with(ArgumentInfo::by_ref);
    }

    pub(crate) fn set_by_refs(&mut self, by_refs: impl Iterator<Item = Ident>) {
        by_refs.for_each(|ident| self.set_by_ref(ident));
    }

    pub(crate) fn is_by_refs(&self, id: &Ident) -> bool {
        self.args
            .get(id)
            .map(|arg| arg.is_by_ref())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod should_implement_is_future_await_logic {
    use super::*;
    use crate::test::*;

    #[fixture]
    fn info() -> ArgumentsInfo {
        let mut a = ArgumentsInfo::default();
        a.set_future(ident("simple"), FutureArg::Define);
        a.set_future(ident("other_simple"), FutureArg::Define);
        a.set_future(ident("awaited"), FutureArg::Await);
        a.set_future(ident("other_awaited"), FutureArg::Await);
        a.set_future(ident("none"), FutureArg::None);
        a
    }

    #[rstest]
    fn no_matching_ident(info: ArgumentsInfo) {
        assert!(!info.is_future_await(&ident("some")));
        assert!(!info.is_future_await(&ident("simple")));
        assert!(!info.is_future_await(&ident("none")));
    }

    #[rstest]
    fn matching_ident(info: ArgumentsInfo) {
        assert!(info.is_future_await(&ident("awaited")));
        assert!(info.is_future_await(&ident("other_awaited")));
    }

    #[rstest]
    fn global_matching_future_ident(mut info: ArgumentsInfo) {
        info.set_global_await(true);
        assert!(info.is_future_await(&ident("simple")));
        assert!(info.is_future_await(&ident("other_simple")));
        assert!(info.is_future_await(&ident("awaited")));

        assert!(!info.is_future_await(&ident("some")));
        assert!(!info.is_future_await(&ident("none")));
    }
}
