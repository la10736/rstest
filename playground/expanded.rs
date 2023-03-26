#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2018::*;
#[macro_use]
extern crate std;
use rstest::*;
#[allow(non_camel_case_types)]
struct two_args_mix_fixture {}
impl two_args_mix_fixture {
    #[allow(unused_mut)]
    pub async fn get(four: impl std::future::Future<Output = u32>, two: u32) -> u32 {
        two_args_mix_fixture(four, two).await
    }
    pub async fn default() -> u32 {
        let four = async { 4 };
        let two = 2;
        Self::get(four, two).await
    }
    #[allow(unused_mut)]
    pub async fn partial_1(four: impl std::future::Future<Output = u32>) -> u32 {
        let two = 2;
        Self::get(four, two).await
    }
    #[allow(unused_mut)]
    pub async fn partial_2(four: impl std::future::Future<Output = u32>, two: u32) -> u32 {
        Self::get(four, two).await
    }
}
#[allow(dead_code)]
async fn two_args_mix_fixture(four: impl std::future::Future<Output = u32>, two: u32) -> u32 {
    let four = four.await;
    {
        four * 10 + two
    }
}
extern crate test;
#[cfg(test)]
#[rustc_test_marker = "use_two_args_mix_fixture_inject_both"]
pub const use_two_args_mix_fixture_inject_both: test::TestDescAndFn = test::TestDescAndFn {
    desc: test::TestDesc {
        name: test::StaticTestName("use_two_args_mix_fixture_inject_both"),
        ignore: false,
        ignore_message: ::core::option::Option::None,
        compile_fail: false,
        no_run: false,
        should_panic: test::ShouldPanic::No,
        test_type: test::TestType::UnitTest,
    },
    testfn: test::StaticTestFn(|| test::assert_test_result(use_two_args_mix_fixture_inject_both())),
};
fn use_two_args_mix_fixture_inject_both() {
    async fn use_two_args_mix_fixture_inject_both(
        two_args_mix_fixture: impl std::future::Future<Output = u32>,
    ) {
        let two_args_mix_fixture = two_args_mix_fixture.await;
        {
            match (&31, &two_args_mix_fixture) {
                (left_val, right_val) => {
                    if !(*left_val == *right_val) {
                        let kind = ::core::panicking::AssertKind::Eq;
                        ::core::panicking::assert_failed(
                            kind,
                            &*left_val,
                            &*right_val,
                            ::core::option::Option::None,
                        );
                    }
                }
            };
        }
    }
    let two_args_mix_fixture = two_args_mix_fixture::partial_2(async { 3 }, 1);
    #[allow(unnameable_test_items)]
    {
        struct _RstestInnerDataContext<RSTEST_TYPE_0: std::future::Future<Output = u32>> {
            two_args_mix_fixture: RSTEST_TYPE_0,
        }
        const _RSTEST_INNER_DATA_CONTEXT: ::std::thread::LocalKey<
            std::cell::RefCell<Option<_RstestInnerDataContext<_>>>,
        > = {
            #[inline]
            fn __init() -> std::cell::RefCell<Option<_RstestInnerDataContext<_>>> {
                std::cell::RefCell::new(None)
            }
            #[inline]
            unsafe fn __getit(
                init: ::std::option::Option<
                    &mut ::std::option::Option<
                        std::cell::RefCell<Option<_RstestInnerDataContext<_>>>,
                    >,
                >,
            ) -> ::std::option::Option<
                &'static std::cell::RefCell<Option<_RstestInnerDataContext<_>>>,
            > {
                #[thread_local]
                static __KEY: ::std::thread::__LocalKeyInner<
                    std::cell::RefCell<Option<_RstestInnerDataContext<_>>>,
                > = ::std::thread::__LocalKeyInner::<
                    std::cell::RefCell<Option<_RstestInnerDataContext<_>>>,
                >::new();
                #[allow(unused_unsafe)]
                unsafe {
                    __KEY.get(move || {
                        if let ::std::option::Option::Some(init) = init {
                            if let ::std::option::Option::Some(value) = init.take() {
                                return value;
                            } else if true {
                                ::core::panicking::panic_fmt(format_args!(
                                    "internal error: entered unreachable code: {0}",
                                    format_args!("missing default value")
                                ));
                            }
                        }
                        __init()
                    })
                }
            }
            unsafe { ::std::thread::LocalKey::new(__getit) }
        };;
        _RSTEST_INNER_DATA_CONTEXT.with(|r| {
            r.replace(Some(_RstestInnerDataContext::<_> {
                two_args_mix_fixture,
            }))
        });
        extern crate test;
        #[cfg(test)]
        #[rustc_test_marker = "wrap"]
        pub const wrap: test::TestDescAndFn = test::TestDescAndFn {
            desc: test::TestDesc {
                name: test::StaticTestName("wrap"),
                ignore: false,
                ignore_message: ::core::option::Option::None,
                compile_fail: false,
                no_run: false,
                should_panic: test::ShouldPanic::No,
                test_type: test::TestType::UnitTest,
            },
            testfn: test::StaticTestFn(|| test::assert_test_result(wrap())),
        };
        #[inline]
        fn wrap() {
            let body = async {
                let _RstestInnerDataContext::<_> {
                    two_args_mix_fixture,
                } = _RSTEST_INNER_DATA_CONTEXT
                    .with(|r| r.replace(None))
                    .unwrap();
                use_two_args_mix_fixture_inject_both(two_args_mix_fixture).await
            };
            let mut body = body;
            #[allow(unused_mut)]
            let mut body = unsafe { ::tokio::macros::support::Pin::new_unchecked(&mut body) };
            let body: ::std::pin::Pin<&mut dyn ::std::future::Future<Output = ()>> = body;
            #[allow(clippy::expect_used, clippy::diverging_sub_expression)]
            {
                return tokio::runtime::Builder::new_current_thread()
                    .enable_all()
                    .build()
                    .expect("Failed building the Runtime")
                    .block_on(body);
            }
        }
        wrap()
    }
}
#[cfg(test)]
async fn simple(val: u32) {
    {
        match (&42, &val) {
            (left_val, right_val) => {
                if !(*left_val == *right_val) {
                    let kind = ::core::panicking::AssertKind::Eq;
                    ::core::panicking::assert_failed(
                        kind,
                        &*left_val,
                        &*right_val,
                        ::core::option::Option::None,
                    );
                }
            }
        };
    }
}
#[cfg(test)]
mod simple {
    use super::*;
    extern crate test;
    #[cfg(test)]
    #[rustc_test_marker = "simple::val_1_42"]
    pub const val_1_42: test::TestDescAndFn = test::TestDescAndFn {
        desc: test::TestDesc {
            name: test::StaticTestName("simple::val_1_42"),
            ignore: false,
            ignore_message: ::core::option::Option::None,
            compile_fail: false,
            no_run: false,
            should_panic: test::ShouldPanic::No,
            test_type: test::TestType::UnitTest,
        },
        testfn: test::StaticTestFn(|| test::assert_test_result(val_1_42())),
    };
    #[allow(non_snake_case)]
    fn val_1_42() {
        let val = 42;
        #[allow(unnameable_test_items)]
        {
            struct _RstestInnerDataContext {
                val: u32,
            }
            const _RSTEST_INNER_DATA_CONTEXT: ::std::thread::LocalKey<
                std::cell::RefCell<Option<_RstestInnerDataContext>>,
            > = {
                #[inline]
                fn __init() -> std::cell::RefCell<Option<_RstestInnerDataContext>> {
                    std::cell::RefCell::new(None)
                }
                #[inline]
                unsafe fn __getit(
                    init: ::std::option::Option<
                        &mut ::std::option::Option<
                            std::cell::RefCell<Option<_RstestInnerDataContext>>,
                        >,
                    >,
                ) -> ::std::option::Option<
                    &'static std::cell::RefCell<Option<_RstestInnerDataContext>>,
                > {
                    #[thread_local]
                    static __KEY: ::std::thread::__LocalKeyInner<
                        std::cell::RefCell<Option<_RstestInnerDataContext>>,
                    > = ::std::thread::__LocalKeyInner::<
                        std::cell::RefCell<Option<_RstestInnerDataContext>>,
                    >::new();
                    #[allow(unused_unsafe)]
                    unsafe {
                        __KEY.get(move || {
                            if let ::std::option::Option::Some(init) = init {
                                if let ::std::option::Option::Some(value) = init.take() {
                                    return value;
                                } else if true {
                                    ::core::panicking::panic_fmt(format_args!(
                                        "internal error: entered unreachable code: {0}",
                                        format_args!("missing default value")
                                    ));
                                }
                            }
                            __init()
                        })
                    }
                }
                unsafe { ::std::thread::LocalKey::new(__getit) }
            };;
            _RSTEST_INNER_DATA_CONTEXT.with(|r| r.replace(Some(_RstestInnerDataContext { val })));
            extern crate test;
            #[cfg(test)]
            #[rustc_test_marker = "simple::wrap"]
            pub const wrap: test::TestDescAndFn = test::TestDescAndFn {
                desc: test::TestDesc {
                    name: test::StaticTestName("simple::wrap"),
                    ignore: false,
                    ignore_message: ::core::option::Option::None,
                    compile_fail: false,
                    no_run: false,
                    should_panic: test::ShouldPanic::No,
                    test_type: test::TestType::UnitTest,
                },
                testfn: test::StaticTestFn(|| test::assert_test_result(wrap())),
            };
            #[inline]
            fn wrap() {
                async_std::task::block_on(async {
                    {
                        let _RstestInnerDataContext { val } = _RSTEST_INNER_DATA_CONTEXT
                            .with(|r| r.replace(None))
                            .unwrap();
                        simple(val).await
                    }
                })
            }
            wrap()
        }
    }
}
#[rustc_main]
pub fn main() -> () {
    extern crate test;
    test::test_main_static(&[&val_1_42, &use_two_args_mix_fixture_inject_both])
}
