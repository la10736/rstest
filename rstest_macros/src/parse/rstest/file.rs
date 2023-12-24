use thiserror::Error;

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Entry {
    pub(crate) name: std::ffi::OsString,
    pub(crate) tests: Bodies,
}

#[derive(Error, Debug)]
pub(crate) enum ParseError {
    #[error("Invalid json")]
    InvalidJson(#[from] json::JsonError),
    #[error("Should be a object or an array of object")]
    InvalidJsonData,
}

impl Entry {
    pub(crate) fn empty<S: AsRef<std::ffi::OsStr> + ?Sized>(name: &S) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            tests: Default::default(),
        }
    }
    pub(crate) fn bodies(mut self, bodes: Bodies) -> Self {
        self.tests = bodes;
        self
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) enum Bodies {
    Root(String),
    Array(Vec<String>),
}

impl Default for Bodies {
    fn default() -> Self {
        Self::Array(Vec::default())
    }
}

impl Bodies {
    pub(crate) fn parse_json_str(json_str: &str) -> Result<Self, ParseError> {
        match json::parse(&json_str)? {
            json::JsonValue::Object(body) => Ok(Self::root(&json::stringify(body))),
            json::JsonValue::Array(array) if array.iter().all(|j| j.is_object()) => {
                let mut bodies = Self::array();
                for a in array {
                    bodies = bodies.add(&json::stringify(a));
                }
                Ok(bodies)
            }
            _ => Err(ParseError::InvalidJsonData),
        }
    }

    pub(crate) fn root(body: &str) -> Self {
        Self::Root(body.to_string())
    }

    pub(crate) fn array() -> Self {
        Self::Array(Default::default())
    }

    pub(crate) fn add(self, body: &str) -> Self {
        match self {
            Bodies::Root(b) => Bodies::Array(vec![b, body.to_string()]),
            Bodies::Array(mut bodies) => {
                bodies.push(body.to_string());
                Bodies::Array(bodies)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse_json_str_should {
        use super::*;
        use crate::test::{assert_eq, *};

        #[test]
        fn read_an_object_as_single_root_test() {
            let json_str = r#"{"age":42,"first_name":"Alice"}"#;
            assert_eq!(
                Bodies::root(json_str),
                Bodies::parse_json_str(json_str).unwrap()
            );
        }

        #[test]
        fn read_an_array_as_list_of_test_object() {
            let json_str = r#"[
                    {"age":42,"first_name":"Bob"},
                    {"age":24,"first_name":"Alice"}
                ]"#;
            assert_eq!(
                Bodies::array()
                    .add(r#"{"age":42,"first_name":"Bob"}"#)
                    .add(r#"{"age":24,"first_name":"Alice"}"#),
                Bodies::parse_json_str(json_str).unwrap()
            );
        }

        #[rstest]
        #[case("1.2")]
        #[case(r#""hello""#)]
        #[case(r#"null"#)]
        #[case::all_array_items_should_be_object(r#"[{},12]"#)]
        #[should_panic]
        fn raise_error(#[case] json_str: &str) {
            Bodies::parse_json_str(json_str).unwrap();
        }
    }
}
