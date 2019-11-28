use rstest::*;

// struct User {
//     name: String,
//     age: u8,
// }

// impl User {
//     fn new(name: &str, age: u8) -> Self {
//         User { name: name.to_owned(), age}
//     }

//     fn name(&self) -> &str {
//         self.name.as_str()
//     }

//     fn age(&self) -> u8 {
//         self.age
//     }
// }

// #[fixture]
// fn name() -> &'static str {
//     "Alice"
// }

// #[fixture]
// fn age() -> u8 {
//     22
// }

// #[fixture]
// fn user(name: &str, age: u8) -> User {
//     User::new(name, age)
// }

// #[rstest]
// fn is_alice(user: User) {
//     assert_eq!(user.name(), "Alice")
// }

// #[rstest]
// fn is_22(user: User) {
//     assert_eq!(user.age(), 22)
// }

// #[rstest(user("Bob"))]
// fn is_bob(user: User) {
//     assert_eq!(user.name(), "Bob")
// }

// #[rstest(user("", 42))]
// fn is_42(user: User) {
//     assert_eq!(user.age(), 42)
// }


#[rstest_parametrize(number, name, tuple,
    case(42, "FortyTwo", ("minus twelve", -12)),
    case(24, "TwentyFour", ("minus twentyfour", -24))
    ::trace
)]
fn should_fail(number: u32, name: &str, tuple: (&str, i32)) {
    assert!(false);
}

fn main() {
    
}