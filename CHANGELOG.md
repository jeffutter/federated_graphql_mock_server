# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.3] - 2026-04-12

### Fixed
- Entity references now include `@inaccessible` key fields with mock values. Previously, when a subgraph returned an entity stub (e.g., `Team` with `resolvable: false`), only visible key fields were included. The Apollo Router requires all key fields — including `@inaccessible` ones — to chain entity resolution across subgraphs.

## [0.3.2] - 2026-04-12

### Fixed
- Custom scalars (DateTime, Timestamp, Date, Time) now generate valid ISO 8601 strings instead of being treated as object references. Previously, the router's scalar coercion would reject these invalid values, causing null propagation through non-nullable parent fields.

### Added
- `@null(probability: <0.0-1.0>)` directive to control null probability for nullable fields. Use `@null` alone for 50% chance, or specify a probability like `@null(probability: 0.8)` for 80% chance of null.
- `@select` directive now works on custom scalar fields (e.g. `startTime: DateTime! @select(from: ["2024-01-01T00:00:00Z"])`).
