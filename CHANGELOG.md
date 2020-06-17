# Changelog

## 0.1.4

- Fix a panic in the Python parser. When a pipe (`|`) occurred in the format string, the formatter would panic with `"unknown conversion flag"` due to an invalid Regex.

## 0.1.3

- Remove use of deprecated `mem::uninitialized`.
- Add messages to invocations of `unreachable!()`.

## 0.1.2

Implement `std::error::Error` for `dynfmt::Error` (courtesy of @cecton).

## 0.1.1

Fixes broken parsing of python mapping names (e.g. `%(name)s`)

## 0.1.0

Initial Release
