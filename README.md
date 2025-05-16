# GraphQL Federation Development Server

A development server for Apollo Federation that allows you to run and test GraphQL subgraphs locally. This tool helps you develop and test federated GraphQL schemas by providing mock data and automatic schema composition.

## Features

- Run multiple GraphQL subgraphs locally
- Automatic mock data generation for GraphQL types
- Customizable mock data using directives
- Real-time schema watching and reloading
- Supergraph composition with Apollo Federation

## Installation

```bash
cargo install graphql-federation-dev
```

## Usage

```bash
graphql-federation-dev --schemas subgraph1=./schemas/subgraph1.graphql --schemas subgraph2=./schemas/subgraph2.graphql --output ./supergraph.graphql
```

### Command Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--schemas`, `-s` | Schema files in the format `subgraph=file_name.graphql` | Required |
| `--output`, `-o` | Output path for the composed supergraph schema | Required |
| `--port` | Port to run the server on | 8080 |
| `--pretty-traces` | Enable pretty-printed traces for debugging | false |

## Mock Data Directives

The server generates mock data for your GraphQL types. You can customize this data using the following directives:

### `@words(min: Int, max: Int)`

Generates random text with a specified number of words.

```graphql
type User {
  bio: String @words(min: 10, max: 20)
}
```

### `@select(from: [String])`

Selects a random value from a provided list.

```graphql
type Product {
  category: String @select(from: ["Electronics", "Clothing", "Books", "Home"])
}
```

### `@count(min: Int, max: Int)`

Specifies the number of items in a list.

```graphql
type User {
  tags: [String!]! @count(min: 2, max: 5)
}
```

## Example Schema

```graphql
type Query {
  user: User
  products: [Product!]! @count(min: 3, max: 10)
}

type User {
  id: ID!
  username: String @words(min: 1, max: 2)
  name: String @select(from: ["John Doe", "Jane Smith", "Alex Johnson"])
  bio: String @words(min: 10, max: 30)
  favoriteProducts: [Product!]! @count(min: 1, max: 3)
}

type Product {
  id: ID!
  name: String @words(min: 2, max: 4)
  category: String @select(from: ["Electronics", "Clothing", "Books"])
  price: Float
  inStock: Boolean
}
```

## Federation Support

This server supports Apollo Federation directives and features, allowing you to develop and test federated GraphQL schemas locally.

```graphql
type User @key(fields: "id") {
  id: ID!
  username: String
}

type Product @key(fields: "id") {
  id: ID!
  name: String
}
```

## Development

To build and run the project locally:

```bash
cargo build
cargo run -- --schemas example=./example.graphql --output ./supergraph.graphql
```

## License

MIT
