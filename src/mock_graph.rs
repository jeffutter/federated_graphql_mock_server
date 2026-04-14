use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use async_graphql::dynamic::FieldValue;
use async_graphql::{Name, Value as ConstValue};
use fake::faker::lorem::en::Words;
use fake::{Fake, Faker};
use graphql_parser::schema::{
    Definition, Directive, Document, EnumType, Field, InterfaceType, ObjectType, Type,
    TypeDefinition, UnionType, Value,
};
use indexmap::IndexMap;
use rand::RngExt;
use rand::prelude::IndexedRandom;
use uuid::Uuid;

/// Represents a field in a GraphQL schema with its mock configuration
#[derive(Debug, Clone)]
pub enum MockFieldConfig {
    /// A non-nullable field
    NonNull(MockContentConfig),
    /// A nullable field with a probability of being null
    Nullable(f64, MockContentConfig),
}

/// Represents the content configuration for a field
#[derive(Debug, Clone)]
pub enum MockContentConfig {
    /// A scalar type
    Type(Box<MockTypeConfig>),
    /// A list of items
    List(Box<MockListConfig>),
}

/// Configuration for a list field
#[derive(Debug, Clone)]
pub struct MockListConfig {
    /// The content configuration for items in the list
    pub contents: MockContentConfig,
    /// Minimum number of items
    pub min: usize,
    /// Maximum number of items
    pub max: usize,
}

/// Configuration for scalar types
#[derive(Debug, Clone)]
pub enum MockTypeConfig {
    /// String with random words
    RandomString { min: usize, max: usize },
    /// String selected from a predefined list
    SelectString { selection: Vec<String> },
    /// Random integer
    Int,
    /// Random float
    Float,
    /// Random boolean
    Boolean,
    /// Random ID (UUID)
    ID,
    /// ID selected from a predefined list
    SelectID { selection: Vec<String> },
    /// Reference to another type
    Ref(String),
    /// Reference to an enum
    EnumRef(String),
    /// Reference to an interface
    InterfaceRef(String),
    /// Reference to an interface selected from a predefined list
    SelectInterfaceRef { selection: Vec<String> },
    /// Reference to a union
    UnionRef(String),
    /// Union Type selected from a predefined list
    SelectUnionRef { selection: Vec<String> },
    /// Custom scalar type (e.g. DateTime, Date, Time)
    CustomScalar(String),
    /// Custom scalar selected from a predefined list
    SelectCustomScalar { selection: Vec<String> },
}

/// Main struct for resolving mock GraphQL data
pub struct MockGraph {
    /// Maps object names to their field configurations
    objects: Arc<HashMap<String, HashMap<String, MockFieldConfig>>>,
    /// Maps enum names to their possible values
    enums: Arc<HashMap<String, Vec<String>>>,
    /// Maps interface names to implementing types
    interfaces: Arc<HashMap<String, Vec<String>>>,
    /// Maps union names to possible types
    unions: Arc<HashMap<String, Vec<String>>>,
    /// Maps entity type names to their key field name sets (one per @key directive)
    entity_keys: Arc<HashMap<String, Vec<Vec<String>>>>,
}

/// Builder for constructing a MockGraph
pub struct MockGraphBuilder {
    /// Name of the Query object in the source schema
    // Needed because we have to force this to "Query" to satisfy:
    // https://github.com/async-graphql/async-graphql/blob/75a9d14e8f45176a32bac7f458534c05cabd10cc/src/registry/export_sdl.rs#L158-L201
    source_query_name: String,
    source_mutation_name: Option<String>,
    source_subscription_name: Option<String>,
    /// Maps object names to their field configurations
    objects: HashMap<String, HashMap<String, MockFieldConfig>>,
    /// Maps enum names to their possible values
    enums: HashMap<String, Vec<String>>,
    /// Maps interface names to implementing types
    interfaces: HashMap<String, Vec<String>>,
    /// Maps union names to possible types
    unions: HashMap<String, Vec<String>>,
    /// Set of custom scalar type names
    scalars: HashSet<String>,
    /// Maps interface names to their fields
    interface_fields: HashMap<String, HashMap<String, MockFieldConfig>>,
    /// Tracks object types that implement interfaces
    implementers: HashMap<String, HashSet<String>>,
    /// Maps entity type names to their key field name sets (one per @key directive)
    entity_keys: HashMap<String, Vec<Vec<String>>>,
}

impl MockGraphBuilder {
    /// Create a new MockGraphBuilder
    pub fn new(
        source_query_name: String,
        source_mutation_name: Option<String>,
        source_subscription_name: Option<String>,
    ) -> Self {
        Self {
            objects: HashMap::new(),
            enums: HashMap::new(),
            interfaces: HashMap::new(),
            unions: HashMap::new(),
            scalars: HashSet::new(),
            interface_fields: HashMap::new(),
            implementers: HashMap::new(),
            entity_keys: HashMap::new(),
            source_query_name,
            source_mutation_name,
            source_subscription_name,
        }
    }

    /// Register a GraphQL schema document to build the mock configuration
    pub fn register_document<'a>(mut self, doc: &Document<'a, &'a str>) -> Self {
        // First pass: collect all type definitions
        for def in &doc.definitions {
            if let Definition::TypeDefinition(type_def) = def {
                match type_def {
                    TypeDefinition::Enum(enum_type) => {
                        self.register_enum(enum_type);
                    }
                    TypeDefinition::Union(union_type) => {
                        self.register_union(union_type);
                    }
                    TypeDefinition::Interface(interface_type) => {
                        self.register_interface(interface_type);
                    }
                    TypeDefinition::Scalar(scalar_type) => {
                        self.scalars.insert(scalar_type.name.to_string());
                    }
                    _ => {}
                }
            }
        }

        // Second pass: register object types and their fields
        for def in &doc.definitions {
            if let Definition::TypeDefinition(TypeDefinition::Object(obj_type)) = def {
                self.register_object(obj_type);
            }
        }

        self
    }

    /// Register an enum type definition
    fn register_enum<'a>(&mut self, enum_type: &EnumType<'a, &'a str>) {
        let values = enum_type
            .values
            .iter()
            .map(|v| v.name.to_string())
            .collect();
        self.enums.insert(enum_type.name.to_string(), values);
    }

    /// Register a union type definition
    fn register_union<'a>(&mut self, union_type: &UnionType<'a, &'a str>) {
        let types = union_type.types.iter().map(|t| t.to_string()).collect();
        self.unions.insert(union_type.name.to_string(), types);
    }

    /// Register an interface type definition
    fn register_interface<'a>(&mut self, interface_type: &InterfaceType<'a, &'a str>) {
        let name = interface_type.name.to_string();

        // Register interface fields
        let mut fields = HashMap::new();
        for field in &interface_type.fields {
            let field_config = self.register_field(field);
            fields.insert(field.name.to_string(), field_config);
        }

        self.interface_fields.insert(name.clone(), fields);
        self.interfaces.insert(name, Vec::new());
    }

    /// Register an object type definition
    fn register_object<'a>(&mut self, obj_type: &ObjectType<'a, &'a str>) {
        let obj_name = match obj_type.name {
            x if x == self.source_query_name => "Query",
            x if Some(x) == self.source_mutation_name.as_deref() => "Mutation",
            x if Some(x) == self.source_subscription_name.as_deref() => "Subscription",
            x => x,
        }
        .to_string();
        let mut fields = HashMap::new();

        // Register object fields
        for field in &obj_type.fields {
            let field_config = self.register_field(field);
            fields.insert(field.name.to_string(), field_config);
        }

        self.objects.insert(obj_name.clone(), fields);

        // Parse @key directives to track entity key fields
        let key_field_sets: Vec<Vec<String>> = obj_type
            .directives
            .iter()
            .filter(|d| d.name == "key")
            .filter_map(|d| {
                d.arguments.iter().find_map(|(name, value)| {
                    if *name == "fields" {
                        if let Value::String(s) = value {
                            Some(parse_key_fields_string(s))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            })
            .collect();

        if !key_field_sets.is_empty() {
            self.entity_keys.insert(obj_name.clone(), key_field_sets);
        }

        // Register object as implementer of interfaces
        for implements in &obj_type.implements_interfaces {
            let interface_name = implements.to_string();
            self.implementers
                .entry(interface_name.clone())
                .or_default()
                .insert(obj_name.clone());

            self.interfaces
                .entry(interface_name)
                .or_default()
                .push(obj_name.clone());
        }
    }

    /// Register a field definition
    fn register_field<'a>(&self, field: &Field<'a, &'a str>) -> MockFieldConfig {
        let is_non_null = matches!(field.field_type, Type::NonNullType(_));
        let content_config = self.register_type(&field.field_type, &field.directives);

        if is_non_null {
            MockFieldConfig::NonNull(content_config)
        } else {
            let null_prob = self.get_null_directive(&field.directives).unwrap_or(0.0);
            MockFieldConfig::Nullable(null_prob, content_config)
        }
    }

    /// Register a type definition
    fn register_type<'a>(
        &self,
        ty: &Type<'a, &'a str>,
        directives: &[Directive<'a, &'a str>],
    ) -> MockContentConfig {
        match ty {
            Type::NamedType(name) => {
                let type_name = name.to_string();
                let type_config = self.get_type_config(&type_name, directives);
                MockContentConfig::Type(Box::new(type_config))
            }
            Type::ListType(inner) => {
                let inner_config = self.register_type(inner, directives);
                let (min, max) = self.get_count_directive(directives);
                MockContentConfig::List(Box::new(MockListConfig {
                    contents: inner_config,
                    min,
                    max,
                }))
            }
            Type::NonNullType(inner) => self.register_type(inner, directives),
        }
    }

    /// Get configuration for a named type
    fn get_type_config<'a>(
        &self,
        type_name: &str,
        directives: &[Directive<'a, &'a str>],
    ) -> MockTypeConfig {
        match type_name {
            "String" => {
                if let Some(selection) = self.get_select_directive(directives) {
                    MockTypeConfig::SelectString { selection }
                } else {
                    let (min, max) = self.get_words_directive(directives);
                    MockTypeConfig::RandomString { min, max }
                }
            }
            "Int" => MockTypeConfig::Int,
            "Float" => MockTypeConfig::Float,
            "Boolean" => MockTypeConfig::Boolean,
            "ID" => {
                if let Some(selection) = self.get_select_directive(directives) {
                    MockTypeConfig::SelectID { selection }
                } else {
                    MockTypeConfig::ID
                }
            }
            _ => {
                // Check if it's an enum, interface, union, or custom scalar
                if self.enums.contains_key(type_name) {
                    MockTypeConfig::EnumRef(type_name.to_string())
                } else if self.interfaces.contains_key(type_name) {
                    if let Some(selection) = self.get_select_directive(directives) {
                        MockTypeConfig::SelectInterfaceRef { selection }
                    } else {
                        MockTypeConfig::InterfaceRef(type_name.to_string())
                    }
                } else if self.unions.contains_key(type_name) {
                    if let Some(selection) = self.get_select_directive(directives) {
                        MockTypeConfig::SelectUnionRef { selection }
                    } else {
                        MockTypeConfig::UnionRef(type_name.to_string())
                    }
                } else if self.scalars.contains(type_name) {
                    if let Some(selection) = self.get_select_directive(directives) {
                        MockTypeConfig::SelectCustomScalar { selection }
                    } else {
                        MockTypeConfig::CustomScalar(type_name.to_string())
                    }
                } else {
                    // Assume it's an object reference
                    MockTypeConfig::Ref(type_name.to_string())
                }
            }
        }
    }

    /// Extract @select directive values
    fn get_select_directive<'a>(
        &self,
        directives: &[Directive<'a, &'a str>],
    ) -> Option<Vec<String>> {
        directives.iter().find_map(|d| {
            if d.name == "select" {
                d.arguments.iter().find_map(|(name, value)| {
                    if *name == "from" {
                        if let Value::List(values) = value {
                            let strings = values
                                .iter()
                                .filter_map(|v| {
                                    if let Value::String(s) = v {
                                        Some(s.to_string())
                                    } else {
                                        None
                                    }
                                })
                                .collect();
                            Some(strings)
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        })
    }

    /// Extract @words directive values
    fn get_words_directive<'a>(&self, directives: &[Directive<'a, &'a str>]) -> (usize, usize) {
        let mut min = 1;
        let mut max = 3;

        for directive in directives {
            if directive.name == "words" {
                for (name, value) in &directive.arguments {
                    if let Value::Int(val) = value {
                        let val = val.as_i64().unwrap_or(0) as usize;
                        if *name == "min" {
                            min = val;
                        } else if *name == "max" {
                            max = val;
                        }
                    }
                }
            }
        }

        (min, max)
    }

    /// Extract @count directive values
    fn get_count_directive<'a>(&self, directives: &[Directive<'a, &'a str>]) -> (usize, usize) {
        let mut min = 1;
        let mut max = 1;

        for directive in directives {
            if directive.name == "count" {
                for (name, value) in &directive.arguments {
                    if let Value::Int(val) = value {
                        let val = val.as_i64().unwrap_or(0) as usize;
                        if *name == "min" {
                            min = val;
                        } else if *name == "max" {
                            max = val;
                        }
                    }
                }
            }
        }

        (min, max)
    }

    /// Extract @null directive probability
    fn get_null_directive<'a>(&self, directives: &[Directive<'a, &'a str>]) -> Option<f64> {
        directives.iter().find(|d| d.name == "null").map(|d| {
            d.arguments
                .iter()
                .find(|(name, _)| *name == "probability")
                .and_then(|(_, value)| match value {
                    Value::Float(f) => Some(*f),
                    Value::Int(i) => i.as_i64().map(|v| v as f64),
                    _ => None,
                })
                .unwrap_or(0.5)
        })
    }

    /// Build the final MockGraph
    pub fn build(self) -> MockGraph {
        MockGraph {
            objects: Arc::new(self.objects),
            enums: Arc::new(self.enums),
            interfaces: Arc::new(self.interfaces),
            unions: Arc::new(self.unions),
            entity_keys: Arc::new(self.entity_keys),
        }
    }
}

impl MockGraph {
    /// Create a new MockGraphBuilder
    pub fn builder(
        source_query_name: String,
        source_mutation_name: Option<String>,
        source_subscription_name: Option<String>,
    ) -> MockGraphBuilder {
        MockGraphBuilder::new(
            source_query_name,
            source_mutation_name,
            source_subscription_name,
        )
    }

    /// Resolve a field for a given object type
    pub fn resolve_field(&self, obj_type: &str, field_name: &str) -> Option<FieldValue<'_>> {
        if self.interfaces.contains_key(obj_type) {
            return self.resolve_interface_field(obj_type, field_name);
        } else if self.unions.contains_key(obj_type) {
            return self.resolve_union_obj(obj_type);
        }

        // Otherwise, resolve as a regular object field
        let obj_fields = self.objects.get(obj_type)?;
        let field_config = obj_fields.get(field_name)?;

        self.resolve_field_config(field_config)
    }

    /// Resolve a field for an interface type
    fn resolve_interface_field(
        &self,
        interface_type: &str,
        field_name: &str,
    ) -> Option<FieldValue<'_>> {
        // Get a random implementing type
        let implementers = self.interfaces.get(interface_type)?;
        if implementers.is_empty() {
            return None;
        }

        let random_type = implementers.choose(&mut rand::rng())?;
        self.resolve_field(random_type, field_name)
    }

    /// Resolve a field config, always producing a value (ignoring null probability).
    /// Used for entity key fields which must always have values.
    fn resolve_field_config_non_null(&self, config: &MockFieldConfig) -> Option<FieldValue<'_>> {
        let content = match config {
            MockFieldConfig::NonNull(content) | MockFieldConfig::Nullable(_, content) => content,
        };
        self.resolve_content_config(content)
    }

    /// Resolve a complete object.
    ///
    /// Returns a FieldValue without `.with_type()` wrapping. Callers that need
    /// type info for union/interface resolution apply `.with_type()` themselves
    /// (entity resolver, resolve_interface_obj, resolve_union_obj).
    ///
    /// For entity types, pre-populates key field values in the returned object
    /// so they are accessible to field resolvers via the parent_map check.
    pub fn resolve_obj(&self, obj_type: &str) -> Option<FieldValue<'_>> {
        if self.interfaces.contains_key(obj_type) {
            self.resolve_interface_obj(obj_type)
        } else if self.unions.contains_key(obj_type) {
            self.resolve_union_obj(obj_type)
        } else {
            let mut obj_map = IndexMap::new();

            // Pre-populate entity key fields so they are always present in the
            // parent object, including @inaccessible fields needed for federation
            if let Some(key_field_sets) = self.entity_keys.get(obj_type)
                && let Some(obj_fields) = self.objects.get(obj_type)
            {
                for key_field_set in key_field_sets {
                    for field_name in key_field_set {
                        if obj_map.contains_key(field_name.as_str()) {
                            continue;
                        }
                        if let Some(field_config) = obj_fields.get(field_name.as_str())
                            && let Some(field_value) =
                                self.resolve_field_config_non_null(field_config)
                            && let Some(const_value) = field_value.as_value()
                        {
                            obj_map.insert(Name::new(field_name), const_value.clone());
                        }
                    }
                }
            }

            Some(FieldValue::value(ConstValue::Object(obj_map)))
        }
    }

    /// Resolve an interface
    fn resolve_interface_obj(&self, interface_type: &str) -> Option<FieldValue<'_>> {
        let implementers = self.interfaces.get(interface_type)?;
        if implementers.is_empty() {
            return None;
        }

        let random_type = implementers.choose(&mut rand::rng())?;
        let value = self.resolve_obj(random_type)?;
        Some(value.with_type(random_type.to_string()))
    }

    /// Resolve a union
    fn resolve_union_obj(&self, union_type: &str) -> Option<FieldValue<'_>> {
        let member_types = self.unions.get(union_type)?;
        if member_types.is_empty() {
            return None;
        }

        let random_type = member_types.choose(&mut rand::rng())?;
        let value = self.resolve_obj(random_type)?;

        // Use WithType to specify the concrete type
        Some(FieldValue::with_type(value, random_type.to_string()))
    }

    /// Resolve a field configuration
    fn resolve_field_config(&self, config: &MockFieldConfig) -> Option<FieldValue<'_>> {
        match config {
            MockFieldConfig::NonNull(content) => self.resolve_content_config(content),
            MockFieldConfig::Nullable(null_prob, content) => {
                if rand::random::<f64>() < *null_prob {
                    None
                } else {
                    self.resolve_content_config(content)
                }
            }
        }
    }

    /// Resolve content configuration
    fn resolve_content_config(&self, config: &MockContentConfig) -> Option<FieldValue<'_>> {
        match config {
            MockContentConfig::Type(type_config) => self.resolve_type_config(type_config),
            MockContentConfig::List(list_config) => {
                let count = rand::rng().random_range(list_config.min..=list_config.max);
                let mut values = Vec::with_capacity(count);

                for _ in 0..count {
                    if let Some(value) = self.resolve_content_config(&list_config.contents) {
                        values.push(value);
                    }
                }

                Some(values.into())
            }
        }
    }

    /// Resolve type configuration
    fn resolve_type_config(&self, config: &MockTypeConfig) -> Option<FieldValue<'_>> {
        match config {
            MockTypeConfig::RandomString { min, max } => {
                let words: String = Words(*min..*max + 1).fake::<Vec<String>>().join(" ");
                Some(FieldValue::value(ConstValue::String(words)))
            }
            MockTypeConfig::SelectString { selection } => selection
                .choose(&mut rand::rng())
                .map(|s| FieldValue::value(ConstValue::String(s.clone()))),
            MockTypeConfig::Int => {
                let num: i32 = Faker.fake();
                Some(FieldValue::value(ConstValue::Number(num.into())))
            }
            MockTypeConfig::Float => {
                let num: f64 = Faker.fake();
                // Convert f64 to string first since Number doesn't implement From<f64>
                Some(FieldValue::value(ConstValue::String(num.to_string())))
            }
            MockTypeConfig::Boolean => {
                let val: bool = Faker.fake();
                Some(FieldValue::value(ConstValue::Boolean(val)))
            }
            MockTypeConfig::ID => {
                let id = Uuid::new_v4().to_string();
                Some(FieldValue::value(ConstValue::String(id)))
            }
            MockTypeConfig::SelectID { selection } => selection
                .choose(&mut rand::rng())
                .map(|s| FieldValue::value(ConstValue::String(s.clone()))),
            MockTypeConfig::Ref(type_name) => self.resolve_obj(type_name),
            MockTypeConfig::EnumRef(enum_name) => {
                let values = self.enums.get(enum_name)?;
                values
                    .choose(&mut rand::rng())
                    .map(|v| FieldValue::value(ConstValue::Enum(Name::new(v))))
            }
            MockTypeConfig::InterfaceRef(interface_name) => {
                self.resolve_interface_obj(interface_name)
            }
            MockTypeConfig::SelectInterfaceRef { selection } => selection
                .choose(&mut rand::rng())
                .and_then(|s| self.resolve_obj(s).map(|v| v.with_type(s.to_string()))),
            MockTypeConfig::UnionRef(union_name) => self.resolve_union_obj(union_name),
            MockTypeConfig::SelectUnionRef { selection } => selection
                .choose(&mut rand::rng())
                .and_then(|s| self.resolve_obj(s).map(|v| v.with_type(s.to_string()))),
            MockTypeConfig::CustomScalar(scalar_name) => {
                let value = generate_custom_scalar_value(scalar_name);
                Some(FieldValue::value(ConstValue::String(value)))
            }
            MockTypeConfig::SelectCustomScalar { selection } => selection
                .choose(&mut rand::rng())
                .map(|s| FieldValue::value(ConstValue::String(s.clone()))),
        }
    }
}

/// Parse a @key fields string into a list of top-level field names.
///
/// Simple: "resourceUri competitionSlug" -> ["resourceUri", "competitionSlug"]
/// Nested: "product { sku } region" -> ["product", "region"]
fn parse_key_fields_string(fields_str: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut depth = 0usize;

    for token in fields_str.split_whitespace() {
        if depth == 0 && token != "{" && token != "}" {
            result.push(token.to_string());
        }
        depth += token.chars().filter(|c| *c == '{').count();
        depth = depth.saturating_sub(token.chars().filter(|c| *c == '}').count());
    }

    result
}

/// Generate a mock value for a custom scalar based on its name.
/// Well-known scalar names produce valid values:
/// - DateTime/Timestamp → ISO 8601 datetime strings
/// - Date → ISO 8601 date strings
/// - Time → ISO 8601 time strings
/// - Long/BigInt/Int64 → string-encoded integers
///
/// Unknown scalars produce a placeholder string.
fn generate_custom_scalar_value(scalar_name: &str) -> String {
    let name_lower = scalar_name.to_lowercase();
    let mut rng = rand::rng();

    if name_lower.contains("datetime") || name_lower.contains("timestamp") {
        // ISO 8601 datetime: "2024-03-15T14:30:00Z"
        let year = rng.random_range(2020..=2026);
        let month = rng.random_range(1..=12u32);
        let day = rng.random_range(1..=28u32); // 28 to avoid invalid dates
        let hour = rng.random_range(0..=23u32);
        let minute = rng.random_range(0..=59u32);
        let second = rng.random_range(0..=59u32);
        format!("{year:04}-{month:02}-{day:02}T{hour:02}:{minute:02}:{second:02}Z")
    } else if name_lower.contains("date") {
        // ISO 8601 date: "2024-03-15"
        let year = rng.random_range(2020..=2026);
        let month = rng.random_range(1..=12u32);
        let day = rng.random_range(1..=28u32);
        format!("{year:04}-{month:02}-{day:02}")
    } else if name_lower.contains("time") {
        // ISO 8601 time: "14:30:00Z"
        let hour = rng.random_range(0..=23u32);
        let minute = rng.random_range(0..=59u32);
        let second = rng.random_range(0..=59u32);
        format!("{hour:02}:{minute:02}:{second:02}Z")
    } else if name_lower == "long" || name_lower == "bigint" || name_lower == "int64" {
        // 64-bit integer serialized as a JSON string
        rng.random_range(10_000..10_000_000i64).to_string()
    } else {
        // Unknown custom scalar — return the scalar name as a placeholder
        format!("mock-{scalar_name}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use graphql_parser::parse_schema;

    #[test]
    fn test_basic_schema() {
        let schema = r#"
        type Query {
          person: Person
        }

        interface Person {
          id: ID!
        }

        type User implements Person {
          id: ID!
          username: String @words(min: 3, max: 5)
          name: String @select(from: ["john", "james", "fred"])
          numbers: [Int!]! @count(min:2, max: 10)
          color: Color
        }

        type Customer implements Person {
          id: ID!
          name: String
        }

        enum Color {
          RED
          GREEN
          BLUE
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        // Test resolving fields
        let user_name = mock_graph.resolve_field("User", "name");
        assert!(matches!(
            user_name.and_then(|x| x.as_value().cloned()),
            Some(ConstValue::String(name)) if ["john", "james", "fred"].contains(&name.as_str())
        ));

        let user_id = mock_graph.resolve_field("User", "id");
        assert!(user_id.is_some());

        let user_color = mock_graph.resolve_field("User", "color");
        assert!(matches!(
            user_color.and_then(|x| x.as_value().cloned()),
            Some(ConstValue::Enum(color)) if ["RED", "GREEN", "BLUE"].contains(&color.as_str())
        ));
    }

    #[test]
    fn test_custom_scalar_datetime() {
        let schema = r#"
        scalar DateTime

        type Query {
          event: Event
        }

        type Event {
          startsAt: DateTime!
          updatedAt: DateTime
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        // DateTime! should produce a valid ISO 8601 datetime string
        for _ in 0..10 {
            let value = mock_graph.resolve_field("Event", "startsAt");
            let s = match value.and_then(|x| x.as_value().cloned()) {
                Some(ConstValue::String(s)) => s,
                other => panic!("Expected String, got {:?}", other),
            };
            // Validate ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
            assert!(
                s.len() == 20 && s.contains('T') && s.ends_with('Z'),
                "Invalid DateTime format: {s}"
            );
        }
    }

    #[test]
    fn test_custom_scalar_timestamp() {
        let schema = r#"
        scalar Timestamp

        type Query {
          item: Item
        }

        type Item {
          createdAt: Timestamp!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let value = mock_graph.resolve_field("Item", "createdAt");
        let s = match value.and_then(|x| x.as_value().cloned()) {
            Some(ConstValue::String(s)) => s,
            other => panic!("Expected String, got {:?}", other),
        };
        assert!(
            s.contains('T') && s.ends_with('Z'),
            "Timestamp should produce datetime format: {s}"
        );
    }

    #[test]
    fn test_custom_scalar_date() {
        let schema = r#"
        scalar Date

        type Query {
          item: Item
        }

        type Item {
          birthday: Date!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let value = mock_graph.resolve_field("Item", "birthday");
        let s = match value.and_then(|x| x.as_value().cloned()) {
            Some(ConstValue::String(s)) => s,
            other => panic!("Expected String, got {:?}", other),
        };
        // Date format: YYYY-MM-DD (10 chars, no T)
        assert!(
            s.len() == 10 && !s.contains('T'),
            "Invalid Date format: {s}"
        );
    }

    #[test]
    fn test_custom_scalar_unknown() {
        let schema = r#"
        scalar JSON

        type Query {
          item: Item
        }

        type Item {
          data: JSON!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let value = mock_graph.resolve_field("Item", "data");
        let s = match value.and_then(|x| x.as_value().cloned()) {
            Some(ConstValue::String(s)) => s,
            other => panic!("Expected String, got {:?}", other),
        };
        assert_eq!(s, "mock-JSON");
    }

    #[test]
    fn test_custom_scalar_with_select_directive() {
        let schema = r#"
        scalar DateTime

        type Query {
          item: Item
        }

        type Item {
          startTime: DateTime! @select(from: ["2024-01-01T00:00:00Z", "2025-06-15T12:00:00Z"])
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        for _ in 0..10 {
            let value = mock_graph.resolve_field("Item", "startTime");
            let s = match value.and_then(|x| x.as_value().cloned()) {
                Some(ConstValue::String(s)) => s,
                other => panic!("Expected String, got {:?}", other),
            };
            assert!(
                ["2024-01-01T00:00:00Z", "2025-06-15T12:00:00Z"].contains(&s.as_str()),
                "Unexpected value: {s}"
            );
        }
    }

    #[test]
    fn test_custom_scalar_long() {
        let schema = r#"
        scalar Long

        type Query {
          item: Item
        }

        type Item {
          amount: Long!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        // Long! should produce a string-encoded integer
        for _ in 0..10 {
            let value = mock_graph.resolve_field("Item", "amount");
            let s = match value.and_then(|x| x.as_value().cloned()) {
                Some(ConstValue::String(s)) => s,
                other => panic!("Expected String, got {:?}", other),
            };
            let parsed: i64 = s.parse().unwrap_or_else(|_| panic!("Not a valid i64: {s}"));
            assert!(
                (10_000..10_000_000).contains(&parsed),
                "Long value out of expected range: {parsed}"
            );
        }
    }

    #[test]
    fn test_custom_scalar_bigint() {
        let schema = r#"
        scalar BigInt

        type Query {
          item: Item
        }

        type Item {
          total: BigInt!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let value = mock_graph.resolve_field("Item", "total");
        let s = match value.and_then(|x| x.as_value().cloned()) {
            Some(ConstValue::String(s)) => s,
            other => panic!("Expected String, got {:?}", other),
        };
        s.parse::<i64>()
            .unwrap_or_else(|_| panic!("BigInt should produce a valid i64 string: {s}"));
    }

    #[test]
    fn test_custom_scalar_long_does_not_overmatch() {
        let schema = r#"
        scalar LongString

        type Query {
          item: Item
        }

        type Item {
          data: LongString!
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let value = mock_graph.resolve_field("Item", "data");
        let s = match value.and_then(|x| x.as_value().cloned()) {
            Some(ConstValue::String(s)) => s,
            other => panic!("Expected String, got {:?}", other),
        };
        assert_eq!(s, "mock-LongString");
    }

    #[test]
    fn test_parse_key_fields_string_simple() {
        assert_eq!(
            parse_key_fields_string("resourceUri competitionSlug organizationSlug"),
            vec!["resourceUri", "competitionSlug", "organizationSlug"]
        );
    }

    #[test]
    fn test_parse_key_fields_string_single() {
        assert_eq!(parse_key_fields_string("id"), vec!["id"]);
    }

    #[test]
    fn test_parse_key_fields_string_nested() {
        assert_eq!(
            parse_key_fields_string("product { sku } region"),
            vec!["product", "region"]
        );
    }

    #[test]
    fn test_entity_key_fields_prepopulated_in_resolve_obj() {
        let schema = r#"
        schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key", "@inaccessible"]) {
          query: Query
        }

        type Query {
          selection: MarketSelection
        }

        type Team @key(fields: "resourceUri competitionSlug organizationSlug", resolvable: false) {
          resourceUri: String!
          competitionSlug: String! @inaccessible
          organizationSlug: String! @inaccessible
        }

        type MarketSelection @key(fields: "id") {
          id: ID!
          participantV2: Team
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        // resolve_obj for Team should pre-populate all three key fields
        let team_value = mock_graph.resolve_obj("Team");
        assert!(team_value.is_some());
        let team_value = team_value.unwrap();
        let obj = team_value.as_value().unwrap();
        if let ConstValue::Object(map) = obj {
            assert!(map.contains_key("resourceUri"), "missing resourceUri");
            assert!(
                map.contains_key("competitionSlug"),
                "missing competitionSlug"
            );
            assert!(
                map.contains_key("organizationSlug"),
                "missing organizationSlug"
            );
        } else {
            panic!("Expected Object, got {:?}", obj);
        }
    }

    #[test]
    fn test_multiple_key_directives() {
        let schema = r#"
        schema @link(url: "https://specs.apollo.dev/federation/v2.7", import: ["@key"]) {
          query: Query
        }

        type Query {
          product: Product
        }

        type Product @key(fields: "id") @key(fields: "sku brand") {
          id: ID!
          sku: String!
          brand: String!
          name: String
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let product_value = mock_graph.resolve_obj("Product");
        assert!(product_value.is_some());
        let product_value = product_value.unwrap();
        let obj = product_value.as_value().unwrap();
        if let ConstValue::Object(map) = obj {
            assert!(map.contains_key("id"), "missing id from first @key");
            assert!(map.contains_key("sku"), "missing sku from second @key");
            assert!(map.contains_key("brand"), "missing brand from second @key");
            // Non-key field should NOT be pre-populated
            assert!(
                !map.contains_key("name"),
                "non-key field 'name' should not be pre-populated"
            );
        } else {
            panic!("Expected Object, got {:?}", obj);
        }
    }

    #[test]
    fn test_non_entity_type_has_empty_obj() {
        let schema = r#"
        type Query {
          user: User
        }

        type User {
          id: ID!
          name: String
        }
        "#;

        let doc = parse_schema::<&str>(schema).unwrap();
        let mock_graph = MockGraph::builder(String::from("Query"), None, None)
            .register_document(&doc)
            .build();

        let user_value = mock_graph.resolve_obj("User");
        assert!(user_value.is_some());
        let user_value = user_value.unwrap();
        let obj = user_value.as_value().unwrap();
        if let ConstValue::Object(map) = obj {
            assert!(map.is_empty(), "non-entity type should have empty obj map");
        } else {
            panic!("Expected Object, got {:?}", obj);
        }
    }
}
