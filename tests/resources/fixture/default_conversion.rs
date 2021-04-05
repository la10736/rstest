use rstest::{fixture, rstest};
use std::net::{Ipv4Addr, SocketAddr};

#[fixture]
fn base(#[default = "1.2.3.4"] ip: Ipv4Addr, #[default = r#"8080"#] port: u16) -> SocketAddr {
    SocketAddr::new(ip.into(), port)
}

#[rstest]
fn test_base(base: SocketAddr) {
    assert_eq!(base, "1.2.3.4:8080".parse().unwrap());
}
