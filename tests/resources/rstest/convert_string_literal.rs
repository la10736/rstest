use rstest::*;
use std::net::SocketAddr;

#[rstest]
#[case(true, "1.2.3.4:42")]
#[case(true, r#"4.3.2.1:24"#)]
#[case(false, "[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443")]
#[case(false, r#"[2aa1:db8:85a3:8af:1319:8a2e:375:4873]:344"#)]
#[case(false, "this.is.not.a.socket.address")]
#[case(false, r#"this.is.not.a.socket.address"#)]
fn cases(#[case] expected: bool, #[case] addr: SocketAddr) {
    assert_eq!(expected, addr.is_ipv4());
}

#[rstest]
fn values(
    #[values(
        "1.2.3.4:42",
        r#"4.3.2.1:24"#,
        "this.is.not.a.socket.address",
        r#"this.is.not.a.socket.address"#
    )]
    addr: SocketAddr,
) {
    assert!(addr.is_ipv4())
}

#[rstest]
#[case(b"12345")]
fn not_convert_byte_array(#[case] cases: &[u8], #[values(b"abc")] values: &[u8]) {
    assert_eq!(5, cases.len());
    assert_eq!(3, values.len());
}

trait MyTrait {
    fn my_trait(&self) -> u32 {
        42
    }
}

impl MyTrait for &str {}

#[rstest]
#[case("impl", "nothing")]
fn not_convert_impl(#[case] that_impl: impl MyTrait, #[case] s: &str) {
    assert_eq!(42, that_impl.my_trait());
    assert_eq!(42, s.my_trait());
}
