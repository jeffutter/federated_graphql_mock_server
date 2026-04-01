# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Rust CLI tool for local development of Apollo Federation GraphQL subgraphs. It generates mock data automatically, watches schema files for changes, and composes supergraphs using Apollo's Rover CLI.

Three commands: `serve` (run local dev server with mock resolvers), `fetch` (pull schemas from Apollo Studio proposals), `new` (scaffold a federated project). The `serve` command supports mixing real and mocked subgraphs via `-s name=schema.graphql@http://real-url`.

## Build & Test Commands

```shell
cargo test --all-features --workspace        # Run all tests
cargo clippy --all-targets --all-features --workspace -- -D warnings  # Lint (CI denies warnings)
cargo fmt --all -- --check                   # Check formatting
cargo fmt --all                              # Auto-format
cargo build --release                        # Release build (LTO enabled)
```

Nix is available: `nix build`, `nix flake check`.

## Architecture

**Entry point:** `main.rs` — CLI via clap, routes to `serve`/`fetch`/`new` commands.

**Core data flow for `serve`:**
1. `schema_parser.rs` — Nom-based preprocessor normalizes `extend schema @link` directives
2. `schema_loader.rs` — Parses SDL with graphql-parser, builds async-graphql dynamic schemas
3. `schema.rs` — Registers types (objects, interfaces, unions, enums, scalars) with field resolvers using traits (`ApplyDirective`, `ApplyDirectives`, `ApplyArgument`)
4. `mock_graph.rs` — Builder pattern generates mock field values; `MockFieldConfig`/`MockContentConfig`/`MockTypeConfig` enums drive resolution
5. `server.rs` — Axum HTTP server with GraphiQL per subgraph; coordinates schema watching, config updates, and serving via `CancellationToken` and broadcast channels
6. `supergraph_compose.rs` / `supergraph_config.rs` — Wraps Rover CLI for federation composition; caches outputs via SHA-256 hashing

**Key patterns:**
- Handle pattern: `SchemaLoader` owns state, `SchemaLoaderHandle` is the clone-friendly shared reference
- `SchemaWatcher` implements `Stream<Item>` for file change notifications (notify crate)
- Closure-based field resolvers in dynamic schema registration
- Uses a custom branch of async-graphql (`allow-dynamic-schema-subtypes`)

**Custom mock directives** users can put in schemas: `@words(min, max)`, `@select(from: [...])`, `@count(min, max)`, `@null(probability)` (default 0.5 if omitted).

## Contributing

Update CHANGELOG.md under "Unreleased" using [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) categories: Added, Changed, Deprecated, Removed, Fixed, Security.
