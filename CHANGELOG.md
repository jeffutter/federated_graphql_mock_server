# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- `@null(probability: <0.0-1.0>)` directive to control null probability for nullable fields. Use `@null` alone for 50% chance, or specify a probability like `@null(probability: 0.8)` for 80% chance of null.
