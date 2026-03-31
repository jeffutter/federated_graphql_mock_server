use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until},
    character::complete::{char, multispace0},
    combinator::recognize,
};
use rand::{RngExt, distr::Alphanumeric};

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
    // Split the input into parts: before extend schema, the extend schema part, and after
    let (before, extend_schema, after) = match split_on_extend_schema(input) {
        Ok((rest, (before, directive))) => {
            // Successfully found and parsed the extend schema directive
            (before, directive, rest)
        }
        Err(_) => {
            return Ok(input.to_string());
        }
    };

    // Parse the link directive from the extend schema part
    let link_directive = match parse_link_directive(extend_schema) {
        Ok((_, directive)) => directive,
        Err(_) => return Ok(input.to_string()), // Return original if parsing fails
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
        before, link_directive, after, random_field
    );

    Ok(new_schema)
}

// Split the input into parts: before the extend schema directive and the directive itself
fn split_on_extend_schema(input: &str) -> IResult<&str, (&str, &str)> {
    let (input_after_before, before) = take_until("extend schema")(input)?;

    // Apply the parser directly
    let (rest, (_extend, _, directive)) = (tag("extend schema"), multispace0, |i| {
        parse_link_directive(i)
    })
        .parse(input_after_before)?;

    Ok((rest, (before, directive)))
}

// Parse and capture just the @link(...) part
fn parse_link_directive(input: &str) -> IResult<&str, &str> {
    recognize((tag("@link"), multispace0, delimited_parentheses)).parse(input)
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
}
