use rstest_test::{assert_in, assert_not_in};

#[test]
fn assert_in_should_find_string() {
    assert_in!(
        "messege that firtspart
        continue.
        some other",
        "part
        cont"
    );
}

#[test]
#[should_panic]
fn assert_in_should_panic_if_no_string_in_message() {
    assert_in!(
        "messege that firtspart
        continue.
        some other",
        "something else"
    );
}

#[test]
#[should_panic]
fn assert_in_should_panic_if_empty() {
    assert_in!("", "a");
}

#[test]
#[should_panic(expected = "supercalifragili")]
fn assert_in_should_yield_text_if_fail() {
    assert_in!("supercalifragili", "x");
}

#[test]
#[should_panic(expected = "xxx")]
fn assert_in_should_yield_message_if_fail() {
    assert_in!("supercalifragili", "xxx");
}

#[test]
#[should_panic]
fn assert_not_in_should_find_string() {
    assert_not_in!(
        "messege that firtspart
        continue.
        some other",
        "part
        cont"
    );
}

#[test]
fn assert_not_in_pass_if_no_string_in_message() {
    assert_not_in!(
        "messege that firtspart
        continue.
        some other",
        "something else"
    );
}

#[test]
fn assert_not_in_should_pass_if_empty() {
    assert_not_in!("", "a");
}

#[test]
#[should_panic(expected = "supercalifragili")]
fn assert_not_in_should_yield_text_if_fail() {
    assert_not_in!("supercalifragili", "rcal");
}

#[test]
#[should_panic(expected = "rcal")]
fn assert_not_in_should_yield_message_if_fail() {
    assert_not_in!("supercalifragili", "rcal");
}
