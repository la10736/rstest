use rstest::rstest;
use rstest_reuse::{self, *};

// Here we define the template. This define 
// * The test set name to `two_simple_cases`
// * cases: here two cases that feed the `a`, `b` values
#[template]
#[rstest(a,  b, 
    case(2, 2), 
    case(4/2, 2),
    )
]
fn two_simple_cases(a: u32, b: u32) {}

// Here we apply the `two_simple_cases` template: That is expanded in
// #[template]
// #[rstest(a,  b, 
//     case(2, 2), 
//     case(4/2, 2),
//     )
// ]
// fn it_works(a: u32, b: u32) {
//     assert!(a == b);
// }
#[apply(two_simple_cases)]
fn it_works(a: u32, b: u32) {
    assert!(a == b);
}


// Here we reuse the `two_simple_cases` template to create two other tests
#[apply(two_simple_cases)]
fn it_fail(a: u32, b: u32) {
    assert!(a != b);
}
