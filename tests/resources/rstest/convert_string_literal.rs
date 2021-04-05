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
