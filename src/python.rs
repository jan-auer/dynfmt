//! Implementation for old-style Python format strings.
//!
//! See [`PythonFormat`] for more information.
//!
//! [`PythonFormat`]: struct.PythonFormat.html

use regex::{CaptureMatches, Captures, Regex};

use crate::{Alignment, ArgumentResult, ArgumentSpec, Count, Error, Format, FormatType, Position};

lazy_static::lazy_static! {
/// The regular expression used for parsing python format strings.
    static ref PYTHON_RE: Regex = Regex::new(r"(?x)
        %
        (?:\((?P<key>\w+)\))?         # Mapping key
        (?P<flags>[\#0\- +]*)?        # Conversion flags
        (?P<width>\*|\d+)?            # Minimum field width
        (?:.(?P<precision>\*|\d+))?   # Precision after decimal point
        [hlL]*                        # Ignored length modifier
        (?P<type>[diouxXeEfFgGcrs%])  # Conversion type
    ").unwrap();
}

fn parse_next(captures: Captures<'_>) -> ArgumentResult<'_> {
    let group = captures.get(0).unwrap();

    let position = captures
        .name("key")
        .map(|m| Position::Key(m.as_str()))
        .unwrap_or_else(|| Position::Auto);

    let format = match &captures["type"] {
        "d" | "i" | "u" => FormatType::Display,
        "o" => FormatType::Octal,
        "x" => FormatType::LowerHex,
        "X" => FormatType::UpperHex,
        "e" => FormatType::LowerExp,
        "E" => FormatType::UpperExp,
        "f" | "F" | "g" | "G" => FormatType::Display,
        "c" | "s" => FormatType::Display,
        "r" => FormatType::Object,
        "%" => FormatType::Literal("%"),
        s => return Err(Error::BadFormat(s.chars().next().unwrap_or_default())),
    };

    let mut alternate = false;
    let mut pad_zero = false;
    let mut alignment = Alignment::Right;
    let mut sign = false;

    if let Some(flags) = captures.name("flags") {
        for flag in flags.as_str().chars() {
            match flag {
                '#' => alternate = true,
                '0' => pad_zero = true,
                '-' => alignment = Alignment::Left,
                ' ' => (), // blank between sign and number, not supported
                '+' => sign = true,
                c => unreachable!("unknown conversion flag \"{}\"", c),
            }
        }
    }

    let width = captures.name("width").and_then(|m| match m.as_str() {
        "*" => Some(Count::Ref(Position::Auto)),
        value => value.parse().ok().map(Count::Value),
    });

    let precision = captures.name("precision").and_then(|m| match m.as_str() {
        "*" => Some(Count::Ref(Position::Auto)),
        value => value.parse().ok().map(Count::Value),
    });

    let spec = ArgumentSpec::new(group.start(), group.end())
        .with_position(position)
        .with_format(format)
        .with_alternate(alternate)
        .with_zeros(pad_zero)
        .with_alignment(alignment)
        .with_sign(sign)
        .with_width(width)
        .with_precision(precision);

    Ok(spec)
}

/// Format argument iterator for [`PythonFormat`].
///
/// [`PythonFormat`]: struct.PythonFormat.html
#[derive(Debug)]
pub struct PythonIter<'f> {
    captures: CaptureMatches<'static, 'f>,
}

impl<'f> PythonIter<'f> {
    fn new(format: &'f str) -> Self {
        PythonIter {
            captures: PYTHON_RE.captures_iter(format),
        }
    }
}

impl<'f> Iterator for PythonIter<'f> {
    type Item = ArgumentResult<'f>;

    fn next(&mut self) -> Option<Self::Item> {
        self.captures.next().map(parse_next)
    }
}

/// Format implementation for old-style Python formatting.
///
/// Python uses a syntax similar to `sprintf` in the C language. Each format argument contains two
/// or more characters and has the following components, which must occur in this order:
///
///  1. The `'%'` character, which marks the start of the specifier.
///  2. Mapping key (optional), consisting of a parenthesised sequence of characters (for example,
///     `(somename)`).
///  3. Conversion flags (optional), which affect the result of some conversion types.
///  4. Minimum field width (optional). If specified as an `'*'` (asterisk), the actual width is
///     read from the next element of the tuple in values, and the object to convert comes after the
///     minimum field width and optional precision.
///  5. Precision (optional), given as a `'.'` (dot) followed by the precision. If specified as
///     `'*'` (an asterisk), the actual width is read from the next element of the tuple in values,
///     and the value to convert comes after the precision.
///  6. Length modifier (optional).
///  7. Conversion type.
///
/// Most of the conversion types are mapped to the standard `Display` trait. The `%r` conversion
/// type is implemented as JSON, if the `json` feature is active and will otherwise error.
///
/// For the full specification, please refer to the [Python string formatting docs].
///
/// # Example
///
/// ```rust
/// use dynfmt::{Format, PythonFormat};
///
/// let formatted = PythonFormat.format("hello, %s", &["world"]);
/// assert_eq!("hello, world", formatted.expect("formatting failed"));
/// ```
///
/// [Python string formatting docs]: https://docs.python.org/2/library/stdtypes.html#string-formatting-operations
#[derive(Debug)]
pub struct PythonFormat;

impl<'f> Format<'f> for PythonFormat {
    type Iter = PythonIter<'f>;

    fn iter_args(&self, format: &'f str) -> Result<Self::Iter, Error<'f>> {
        Ok(PythonIter::new(format))
    }
}
