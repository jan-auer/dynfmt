# dynfmt - Dynamic Formatting in Rust

`dynfmt` provides several implementations for formats that implement a subset of
the [`std::fmt`] facilities. Parsing of the format string and arguments checks
are performed at runtime. There is also the option to implement new formats.

The public API is exposed via the [`Format`] trait, which contains formatting
helper functions and lower-level utilities to interface with format strings. See
the Features section for a list of provided implementations.

## Usage

```rust
use dynfmt::{Format, NoopFormat};

let formatted = NoopFormat.format("hello, world", &["unused"]);
assert_eq!("hello, world", formatted.expect("formatting failed"));
```

See the [Documentation](https://docs.rs/dynfmt) for more information.

[`std::fmt`]: https://doc.rust-lang.org/stable/std/fmt/
[`format`]: https://docs.rs/dynfmt/latest/dynfmt/trait.Format.html
