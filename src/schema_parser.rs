use std::fmt;

use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until},
    character::complete::{char, multispace0},
    combinator::recognize,
};
use rand::{RngExt, distr::Alphanumeric};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FederationVersion {
    pub major: u32,
    pub minor: u32,
}

impl fmt::Display for FederationVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.0", self.major, self.minor)
    }
}

/// Extract the federation version from a schema's `@link` directive URL.
///
/// Looks for `specs.apollo.dev/federation/vMAJOR.MINOR` in the SDL and parses
/// the version. If the schema uses `@connect` (from the connect spec), the
/// minimum version is bumped to 2.12 since rover's composition plugin requires
/// it. Returns `None` if no federation `@link` is found.
pub fn extract_federation_version(sdl: &str) -> Option<FederationVersion> {
    let marker = "specs.apollo.dev/federation/v";
    let start = sdl.find(marker)? + marker.len();
    let rest = &sdl[start..];
    let dot = rest.find('.')?;
    let major: u32 = rest[..dot].parse().ok()?;
    let minor_end = rest[dot + 1..]
        .find(|c: char| !c.is_ascii_digit())
        .unwrap_or(rest.len() - dot - 1);
    let minor: u32 = rest[dot + 1..dot + 1 + minor_end].parse().ok()?;

    let mut version = FederationVersion { major, minor };

    // The connect spec requires federation 2.12+ for composition
    let connect_min = FederationVersion {
        major: 2,
        minor: 12,
    };
    if sdl.contains("specs.apollo.dev/connect/") && version < connect_min {
        version = connect_min;
    }

    Some(version)
}

/*
* This is mainly used to handle Federated GraphQL schemas that start with `extend schema @link..`
* without having a `schema` defined to extend
*/

/// Parses a GraphQL schema and handles the `extend schema @link` directive
pub fn process_schema(input: &str) -> anyhow::Result<String> {
    // Check if we need to process the schema
    if !input.contains("extend schema") {
        return Ok(input.to_string());
    }

    // Try to parse and transform the schema
    let result = transform_schema(input)?;
    Ok(result)
}

fn transform_schema(input: &str) -> anyhow::Result<String> {
    // Split the input into parts: before extend schema, the directives, and after
    let (before, directives_str, after) = match split_on_extend_schema(input) {
        Ok((rest, (before, directives))) => (before, directives, rest),
        Err(_) => {
            return Ok(input.to_string());
        }
    };

    // Check if we need to add a default Query type
    let needs_query = !input.contains("type Query");

    // Generate a random string for the field name if needed
    let random_field = if needs_query {
        let random_string: String = rand::rng()
            .sample_iter(&Alphanumeric)
            .take(10)
            .map(char::from)
            .collect();
        format!("\ntype Query {{ _default_{}: String }}", random_string)
    } else {
        String::new()
    };

    let new_schema = format!(
        "{}schema {} {{ query: Query }}{}{}",
        before, directives_str, after, random_field
    );

    Ok(new_schema)
}

// Split the input into parts: before the extend schema directive and all directives after it
fn split_on_extend_schema(input: &str) -> IResult<&str, (&str, &str)> {
    let (input_after_before, before) = take_until("extend schema")(input)?;

    // Parse "extend schema" followed by whitespace
    let (rest, (_extend, _)) = (tag("extend schema"), multispace0).parse(input_after_before)?;

    // Parse all consecutive @-directives (e.g., @link(...), @source(...))
    let (rest, directives) = parse_all_directives(rest)?;

    Ok((rest, (before, directives)))
}

// Parse and capture all consecutive @-directives with their arguments
fn parse_all_directives(input: &str) -> IResult<&str, &str> {
    let start = input;
    let mut remaining = input;

    loop {
        // Skip whitespace
        let (after_ws, _) = multispace0(remaining)?;
        // Try to parse an @-directive
        match parse_directive(after_ws) {
            Ok((rest, _)) => {
                remaining = rest;
            }
            Err(_) => break,
        }
    }

    if remaining.as_ptr() == start.as_ptr() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }

    let consumed_len = remaining.as_ptr() as usize - start.as_ptr() as usize;
    let directives = &start[..consumed_len];
    Ok((remaining, directives))
}

// Parse a single @name(...) directive
fn parse_directive(input: &str) -> IResult<&str, &str> {
    recognize((
        char('@'),
        nom::character::complete::alpha1,
        multispace0,
        delimited_parentheses,
    ))
    .parse(input)
}

// Helper to parse content within balanced parentheses
fn delimited_parentheses(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('(')(input)?;

    let mut depth = 1;
    let mut pos = 0;

    while depth > 0 && pos < input.len() {
        match input.chars().nth(pos) {
            Some('(') => depth += 1,
            Some(')') => depth -= 1,
            _ => {}
        }
        if depth > 0 {
            pos += 1;
        }
    }

    if depth == 0 {
        let content = &input[..pos];
        let remaining = &input[pos + 1..]; // +1 to skip the closing paren
        Ok((remaining, content))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Char,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extend_schema_link_same_line() {
        let input = r#"extend schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])

type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert!(
            !result.contains("extend schema"),
            "should transform extend schema, got: {result}"
        );
        assert!(
            result.contains("schema @link"),
            "should contain schema @link, got: {result}"
        );
    }

    #[test]
    fn test_extend_schema_link_multiline() {
        let input = r#"extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])

type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert!(
            !result.contains("extend schema"),
            "should transform extend schema, got: {result}"
        );
        assert!(
            result.contains("schema @link"),
            "should contain schema @link, got: {result}"
        );
    }

    #[test]
    fn test_extend_schema_with_directive_declaration_before() {
        let input = r#"directive @contact(name: String!, description: String, url: String) on SCHEMA

extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])

type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert!(
            !result.contains("extend schema"),
            "should transform extend schema, got: {result}"
        );
        assert!(
            result.contains("schema @link"),
            "should contain schema @link, got: {result}"
        );
        assert!(
            result.contains("directive @contact"),
            "should preserve directive declaration, got: {result}"
        );
    }

    #[test]
    fn test_extend_schema_multiple_directives() {
        let input = r#"extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.11", import: ["@key"])
  @link(url: "https://specs.apollo.dev/connect/v0.2", import: ["@source", "@connect"])
  @source(
    name: "identity"
    http: {
      baseURL: "http://example.com/connect"
    }
  )

type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert!(
            !result.contains("extend schema"),
            "should transform extend schema, got: {result}"
        );
        assert!(
            result.contains("schema @link"),
            "should contain schema @link, got: {result}"
        );
        assert!(
            result.contains("@source("),
            "should preserve @source directive, got: {result}"
        );
        assert!(
            result.contains("{ query: Query }"),
            "should contain query definition, got: {result}"
        );
    }

    #[test]
    fn test_no_extend_schema_passthrough() {
        let input = r#"type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert_eq!(result, input);
    }

    #[test]
    fn test_schema_without_extend_passthrough() {
        let input = r#"schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) {
  query: Query
}

type Query {
  hello: String
}"#;
        let result = process_schema(input).unwrap();
        assert_eq!(
            result, input,
            "non-extend schema should pass through unchanged"
        );
    }

    #[test]
    fn test_extract_federation_version_basic() {
        let sdl = r#"extend schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key"])"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion { major: 2, minor: 7 })
        );
    }

    #[test]
    fn test_extract_federation_version_schema_link() {
        let sdl = r#"schema @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key"]) {
  query: Query
}"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion { major: 2, minor: 3 })
        );
    }

    #[test]
    fn test_extract_federation_version_no_link() {
        let sdl = r#"type Query { hello: String }"#;
        assert_eq!(extract_federation_version(sdl), None);
    }

    #[test]
    fn test_extract_federation_version_non_federation_link() {
        let sdl = r#"extend schema @link(url: "https://specs.apollo.dev/link/v1.0")"#;
        assert_eq!(extract_federation_version(sdl), None);
    }

    #[test]
    fn test_extract_federation_version_multiple_links() {
        let sdl = r#"extend schema
  @link(url: "https://specs.apollo.dev/link/v1.0")
  @link(url: "https://specs.apollo.dev/federation/v2.11", import: ["@key"])"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion {
                major: 2,
                minor: 11
            })
        );
    }

    #[test]
    fn test_extract_federation_version_double_digit_minor() {
        let sdl = r#"extend schema @link(url: "https://specs.apollo.dev/federation/v2.11", import: ["@key"])"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion {
                major: 2,
                minor: 11
            })
        );
    }

    #[test]
    fn test_extract_federation_version_url_after_import() {
        let sdl = r#"schema @link(import : ["@key"], url : "https://specs.apollo.dev/federation/v2.7") {"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion { major: 2, minor: 7 })
        );
    }

    #[test]
    fn test_federation_version_ordering() {
        let v23 = FederationVersion { major: 2, minor: 3 };
        let v27 = FederationVersion { major: 2, minor: 7 };
        let v211 = FederationVersion {
            major: 2,
            minor: 11,
        };
        assert!(v23 < v27);
        assert!(v27 < v211);
    }

    #[test]
    fn test_federation_version_display() {
        let v = FederationVersion { major: 2, minor: 7 };
        assert_eq!(format!("={}", v), "=2.7.0");
    }

    #[test]
    fn test_extract_federation_version_connect_bumps_to_2_12() {
        let sdl = r#"extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.11", import: ["@key"])
  @link(url: "https://specs.apollo.dev/connect/v0.2", import: ["@source", "@connect"])"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion {
                major: 2,
                minor: 12
            })
        );
    }

    #[test]
    fn test_extract_federation_version_connect_no_bump_if_already_higher() {
        let sdl = r#"extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.13", import: ["@key"])
  @link(url: "https://specs.apollo.dev/connect/v0.2", import: ["@connect"])"#;
        assert_eq!(
            extract_federation_version(sdl),
            Some(FederationVersion {
                major: 2,
                minor: 13
            })
        );
    }
}
