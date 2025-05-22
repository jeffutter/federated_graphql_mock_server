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
    /// Maps interface names to their fields
    interface_fields: HashMap<String, HashMap<String, MockFieldConfig>>,
    /// Tracks object types that implement interfaces
    implementers: HashMap<String, HashSet<String>>,
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
            interface_fields: HashMap::new(),
            implementers: HashMap::new(),
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
            // Default 0% chance of being null for nullable fields
            MockFieldConfig::Nullable(0.0, content_config)
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
                // Check if it's an enum, interface, or union
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

    /// Build the final MockGraph
    pub fn build(self) -> MockGraph {
        MockGraph {
            objects: Arc::new(self.objects),
            enums: Arc::new(self.enums),
            interfaces: Arc::new(self.interfaces),
            unions: Arc::new(self.unions),
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
    pub fn resolve_field(&self, obj_type: &str, field_name: &str) -> Option<FieldValue> {
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
    ) -> Option<FieldValue> {
        // Get a random implementing type
        let implementers = self.interfaces.get(interface_type)?;
        if implementers.is_empty() {
            return None;
        }

        let random_type = implementers.choose(&mut rand::rng())?;
        self.resolve_field(random_type, field_name)
    }

    /// Resolve a complete object
    pub fn resolve_obj(&self, obj_type: &str) -> Option<FieldValue> {
        // trace!("Resolving object: {}", obj_type);

        // Pop the path when we're done with this object
        let result = {
            // Check if it's an interface or union
            if self.interfaces.contains_key(obj_type) {
                // Clone the path for the recursive call
                self.resolve_interface_obj(obj_type)
            } else if self.unions.contains_key(obj_type) {
                // Clone the path for the recursive call
                self.resolve_union_obj(obj_type)
            } else {
                // This can be an empty object, all the fields will be resolved as the resolvers
                // are called for each field
                Some(
                    FieldValue::value(ConstValue::Object(IndexMap::new()))
                        .with_type(obj_type.to_string()),
                )
            }
        };

        result
    }

    /// Resolve an interface
    fn resolve_interface_obj(&self, interface_type: &str) -> Option<FieldValue> {
        let implementers = self.interfaces.get(interface_type)?;
        if implementers.is_empty() {
            return None;
        }

        let random_type = implementers.choose(&mut rand::rng())?;
        let value = self.resolve_obj(random_type)?;
        Some(value.with_type(random_type.to_string()))
    }

    /// Resolve a union
    fn resolve_union_obj(&self, union_type: &str) -> Option<FieldValue> {
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
    fn resolve_field_config(&self, config: &MockFieldConfig) -> Option<FieldValue> {
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
    fn resolve_content_config(&self, config: &MockContentConfig) -> Option<FieldValue> {
        match config {
            MockContentConfig::Type(type_config) => self.resolve_type_config(type_config),
            MockContentConfig::List(list_config) => {
                let count =
                    rand::Rng::random_range(&mut rand::rng(), list_config.min..=list_config.max);
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
    fn resolve_type_config(&self, config: &MockTypeConfig) -> Option<FieldValue> {
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
        }
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
}
