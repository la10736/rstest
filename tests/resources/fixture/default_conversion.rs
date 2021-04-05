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

#[fixture]
fn byte_array(#[default = b"1234"] some: &[u8]) -> usize {
    some.len()
}

#[rstest]
fn test_byte_array(byte_array: usize) {
    assert_eq!(4, byte_array);
}
