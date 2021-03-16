//! A crate for formatting strings dynamically.
//!
//! `dynfmt` provides several implementations for formats that implement a subset of the
//! [`std::fmt`] facilities. Parsing of the format string and arguments checks are performed at
//! runtime. There is also the option to implement new formats.
//!
//! The public API is exposed via the [`Format`] trait, which contains formatting helper functions
//! and lower-level utilities to interface with format strings. See the Features section for a list
//! of provided implementations.
//!
//! # Usage
//!
//! ```rust
//! use dynfmt::{Format, NoopFormat};
//!
//! let formatted = NoopFormat.format("hello, world", &["unused"]);
//! assert_eq!("hello, world", formatted.expect("formatting failed"));
//! ```
//!
//! See the [`Format`] trait for more methods.
//!
//! # Features
//!
//! This crate ships with a set of features that either activate formatting capabilities or new
//! format implementations:
//!
//!  - `json` **(default)**: Implements the serialization of complex structures via JSON. Certain
//!    formats, such as Python, also have a _representation_ format (`%r`) that makes use of this
//!    feature, if enabled. Without this feature, such values will cause an error.
//!  - `python`: Implements the `printf`-like format that python 2 used for formatting strings. See
//!    [`PythonFormat`] for more information.
//!  - `curly`: A simple format string syntax using curly braces for arguments. Similar to .NET and
//!    Rust, but much less capable. See [`SimpleCurlyFormat`] for mor information.
//!
//! # Extensibility
//!
//! Implement the [`Format`] trait to create a new format. The only required method is `iter_args`,
//! which must return an iterator over [`ArgumentSpec`] structs. Based on the capabilities of the
//! format, the specs can be parameterized with formatting parameters.
//!
//! ```rust
//! use std::str::MatchIndices;
//! use dynfmt::{ArgumentSpec, Format, Error};
//!
//! struct HashFormat;
//!
//! impl<'f> Format<'f> for HashFormat {
//!     type Iter = HashIter<'f>;
//!
//!     fn iter_args(&self, format: &'f str) -> Result<Self::Iter, Error<'f>> {
//!         Ok(HashIter(format.match_indices('#')))
//!     }
//! }
//!
//! struct HashIter<'f>(MatchIndices<'f, char>);
//!
//! impl<'f> Iterator for HashIter<'f> {
//!     type Item = Result<ArgumentSpec<'f>, Error<'f>>;
//!
//!     fn next(&mut self) -> Option<Self::Item> {
//!         self.0.next().map(|(index, _)| Ok(ArgumentSpec::new(index, index + 1)))
//!     }
//! }
//!
//! let formatted = HashFormat.format("hello, #", &["world"]);
//! assert_eq!("hello, world", formatted.expect("formatting failed"));
//! ```
//!
//! [`std::fmt`]: https://doc.rust-lang.org/stable/std/fmt/
//! [`serde::Serialize`]: https://docs.rs/serde/latest/serde/trait.Serialize.html
//! [`Format`]: trait.Format.html
//! [`ArgumentSpec`]: struct.ArgumentSpec.html
//! [`PythonFormat`]: python/struct.PythonFormat.html
//! [`SimpleCurlyFormat`]: curly/struct.SimpleCurlyFormat.html

#![warn(missing_docs)]

use std::borrow::Cow;
use std::fmt;
use std::io;

use thiserror::Error;
use erased_serde::Serialize as Serializable;
use serde::ser::Serialize;

mod formatter;

use crate::formatter::{FormatError, Formatter};

#[cfg(feature = "python")]
pub mod python;
#[cfg(feature = "python")]
pub use crate::python::PythonFormat;

#[cfg(feature = "curly")]
pub mod curly;
#[cfg(feature = "curly")]
pub use crate::curly::SimpleCurlyFormat;

/// Refers to an argument within an argument list.
///
/// During formatting, the formatter will pull arguments from the argument list. Depending on
/// whether the argument list supports indexed or named lookups, this might result in an error. See
/// [`FormatArgs`] for argument list implementations.
///
/// A Position may borrow they key name from the format string.
///
/// [`FormatArgs`]: trait.FormatArgs.html
#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Position<'a> {
    /// The next indexed argument in line.
    ///
    /// The formatter maintains an internal state to keep track of the sequence of auto-arguments.
    /// If an interim access to a specific indexed or named argument occurs, the formatter will
    /// afterwards continue after the last auto argument.
    ///
    /// Requires the argument list to be indexable by numbers.
    Auto,

    /// Index argument at the given offset.
    ///
    /// Requires the argument list to be indexable by numbers.
    Index(usize),

    /// Named argument with the given key.
    ///
    /// Requires the argument list to be indexable by string keys.
    Key(&'a str),
}

impl Default for Position<'_> {
    fn default() -> Self {
        Position::Auto
    }
}

impl fmt::Display for Position<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Position::Auto => write!(f, "{{next}}"),
            Position::Index(index) => write!(f, "{}", index),
            Position::Key(key) => f.write_str(key),
        }
    }
}

/// An error returned during formatting.
///
/// An error may borrow information from the format string.
#[derive(Debug, Error)]
pub enum Error<'a> {
    /// An unknown format character has been specified in the format.
    #[error("unsupported format '{0}'")]
    BadFormat(char),

    /// A custom error during parsing a specific format.
    #[error("error parsing format string: {0}")]
    Parse(Cow<'a, str>),

    /// The format refers to an indexed argument, but the argument list does not support indexed
    /// access.
    #[error("format requires an argument list")]
    ListRequired,

    /// The format refers to a named argument, but the argument list does not support named access.
    #[error("format requires an argument map")]
    MapRequired,

    /// An argument was missing from the argument list.
    #[error("missing argument: {0}")]
    MissingArg(Position<'a>),

    /// An argument could not be formatted in the requested format.
    #[error("argument '{0}' cannot be formatted as {1}")]
    BadArg(Position<'a>, FormatType),

    /// Formatting the data with the requested format resulted in an error.
    #[error("error formatting argument '{0}': {1}")]
    BadData(Position<'a>, String),

    /// An I/O error occurred when writing into the target.
    #[error("{0}")]
    Io(#[from] io::Error),
}

impl<'a> Error<'a> {
    /// Converts the internal formatting error into the public error.
    fn from_serialize(error: FormatError, position: Position<'a>) -> Self {
        match error {
            FormatError::Type(ty) => Error::BadArg(position, ty),
            FormatError::Serde(err) => Error::BadData(position, err),
            FormatError::Io(err) => Error::Io(err),
        }
    }
}

/// Formatting types for arguments.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormatType {
    /// Print the [display] representation of the argument.
    ///
    /// [display]: https://doc.rust-lang.org/stable/std/fmt/trait.Display.html
    Display,

    /// Print the [debug] representation of the argument.
    ///
    /// **This is not yet implemented!**
    ///
    /// [debug]: https://doc.rust-lang.org/stable/std/fmt/trait.Debug.html
    Debug,

    /// Print a structured representation of the argument.
    ///
    /// This will serialize the argument as JSON. If the `json` feature is turned off, an argument
    /// like this will result in an error.
    Object,

    /// Print the [octal] representation of the argument.
    ///
    /// [octal]: https://doc.rust-lang.org/stable/std/fmt/trait.Octal.html
    Octal,

    /// Print the [lower hex] representation of the argument.
    ///
    /// [lower hex]: https://doc.rust-lang.org/stable/std/fmt/trait.LowerHex.html
    LowerHex,

    /// Print the [upper hex] representation of the argument.
    ///
    /// [upper hex]: https://doc.rust-lang.org/stable/std/fmt/trait.UpperHex.html
    UpperHex,

    /// Print the [pointer] representation of the argument.
    ///
    /// [pointer]: https://doc.rust-lang.org/stable/std/fmt/trait.Pointer.html
    Pointer,

    /// Print the [binary] representation of the argument.
    ///
    /// [binary]: https://doc.rust-lang.org/stable/std/fmt/trait.Binary.html
    Binary,

    /// Print the [lower exponential] representation of the argument.
    ///
    /// [lower exponential]: https://doc.rust-lang.org/stable/std/fmt/trait.LowerExp.html
    LowerExp,

    /// Print the [upper exponential] representation of the argument.
    ///
    /// [upper exponential]: https://doc.rust-lang.org/stable/std/fmt/trait.UpperExp.html
    UpperExp,

    /// Print an escaped literal from the format string.
    Literal(&'static str),
}

impl FormatType {
    /// Returns the name of this formatting type.
    pub fn name(self) -> &'static str {
        match self {
            FormatType::Display => "string",
            FormatType::Debug => "debug",
            FormatType::Octal => "octal",
            FormatType::LowerHex => "lower hex",
            FormatType::UpperHex => "upper hex",
            FormatType::Pointer => "pointer",
            FormatType::Binary => "binary",
            FormatType::LowerExp => "lower exp",
            FormatType::UpperExp => "upper exp",
            FormatType::Object => "object",
            FormatType::Literal(s) => s,
        }
    }
}

impl Default for FormatType {
    fn default() -> Self {
        FormatType::Display
    }
}

impl fmt::Display for FormatType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// A serializable argument.
pub type Argument<'a> = &'a dyn Serializable;

/// A container that provides access to indexed or named arguments.
///
/// Instances of this trait can be used as argument lists to format calls. A container may implement
/// either `get_index`, `get_key` or even both.
pub trait FormatArgs {
    /// Returns the argument with the specified index.
    ///
    /// Implement this method if the container supports indexed access. Return `Ok(Some(...))` if
    /// the argument exists, or `Ok(None)` if the index is out of bounds.
    #[allow(unused_variables)]
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        Err(())
    }

    /// Returns the argument with the given name.
    ///
    /// Implement this method if the container supports named access. Return `Ok(Some(...))` if
    /// the argument exists, or `Ok(None)` if the index is out of bounds.
    #[allow(unused_variables)]
    fn get_key(&self, key: &str) -> Result<Option<Argument<'_>>, ()> {
        Err(())
    }
}

impl<T> FormatArgs for Vec<T>
where
    T: Serialize,
{
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        Ok(self.get(index).map(|arg| arg as Argument<'_>))
    }
}

impl<T> FormatArgs for &'_ [T]
where
    T: Serialize,
{
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        Ok(self.get(index).map(|arg| arg as Argument<'_>))
    }
}

macro_rules! impl_args_array {
    ($num:expr) => {
        impl<T> FormatArgs for [T; $num]
        where
            T: Serialize,
        {
            fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
                Ok(self.get(index).map(|arg| arg as Argument<'_>))
            }
        }
    };
}

impl_args_array!(0);
impl_args_array!(1);
impl_args_array!(2);
impl_args_array!(3);
impl_args_array!(4);
impl_args_array!(5);
impl_args_array!(6);
impl_args_array!(7);
impl_args_array!(8);
impl_args_array!(9);
impl_args_array!(10);
impl_args_array!(11);
impl_args_array!(12);
impl_args_array!(13);
impl_args_array!(14);
impl_args_array!(15);
impl_args_array!(16);

impl<T> FormatArgs for std::collections::VecDeque<T>
where
    T: Serialize,
{
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        Ok(self.get(index).map(|arg| arg as Argument<'_>))
    }
}

impl<S, T> FormatArgs for std::collections::BTreeMap<S, T>
where
    S: std::borrow::Borrow<str> + Ord,
    T: Serialize,
{
    fn get_key(&self, key: &str) -> Result<Option<Argument<'_>>, ()> {
        Ok(self.get(key).map(|arg| arg as Argument<'_>))
    }
}

impl<S, T> FormatArgs for std::collections::HashMap<S, T>
where
    S: std::borrow::Borrow<str> + std::hash::Hash + Eq,
    T: Serialize,
{
    fn get_key(&self, key: &str) -> Result<Option<Argument<'_>>, ()> {
        Ok(self.get(key).map(|arg| arg as Argument<'_>))
    }
}

impl<A> FormatArgs for &A
where
    A: FormatArgs,
{
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        (*self).get_index(index)
    }

    fn get_key(&self, key: &str) -> Result<Option<Argument<'_>>, ()> {
        (*self).get_key(key)
    }
}

/// Wrapper that provides a formatter with stateful access to arguments.
struct ArgumentAccess<A> {
    args: A,
    index: usize,
}

impl<A> ArgumentAccess<A>
where
    A: FormatArgs,
{
    /// Creates a new arguments access.
    pub fn new(args: A) -> Self {
        ArgumentAccess { args, index: 0 }
    }

    /// Returns the argument specified by position.
    ///
    /// If the position is [`Position::Auto`], the next indexed argument in line will be pulled and
    /// the index advanced. Otherwise, the auto index remains and the specified argument is
    /// retrieved directly.
    ///
    /// [`Position::Auto`]: enum.Position.html#variant.Auto
    pub fn get_pos<'a>(&mut self, mut position: Position<'a>) -> Result<Argument<'_>, Error<'a>> {
        if position == Position::Auto {
            position = Position::Index(self.index);
            self.index += 1;
        }

        let result = match position {
            Position::Auto => unreachable!("Position::Auto matched twice"),
            Position::Index(index) => self.args.get_index(index).map_err(|()| Error::ListRequired),
            Position::Key(key) => self.args.get_key(key).map_err(|()| Error::MapRequired),
        };

        result.and_then(|opt| opt.ok_or_else(|| Error::MissingArg(position)))
    }
}

/// Specifies the alignment of an argument when formatted with a specific width.
///
/// Defaults to `Alignment::Right`.
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Alignment {
    Left,
    Center,
    Right,
}

impl Default for Alignment {
    fn default() -> Self {
        Alignment::Right
    }
}

/// The value of a formatting parameter, used within [`ArgumentSpec`].
///
/// [`ArgumentSpec`]: struct.ArgumentSpec.html
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Count<'a> {
    /// The literal value as specified in the format string.
    Value(usize),
    /// Reference to an argument in the argument list.
    Ref(Position<'a>),
}

/// The format specification for a single argument in the format string, created by
/// [`Format::iter_args`].
///
/// The argument spec may borrow data from the format string, e.g. when referring to named arguments
/// in the argument list.
///
/// [`Format::iter_args`]: trait.Format.html#tymethod.iter_args
#[derive(Debug)]
pub struct ArgumentSpec<'a> {
    range: (usize, usize),
    position: Position<'a>,
    format: FormatType,
    alternate: bool,
    add_sign: bool,
    pad_zero: bool,
    fill_char: char,
    alignment: Alignment,
    width: Option<Count<'a>>,
    precision: Option<Count<'a>>,
}

impl<'a> ArgumentSpec<'a> {
    /// Creates a new argument specification with default values.
    ///
    /// The `start` and `end` parameters denote the exclusive range of this specification in the
    /// format string. E.g. for a format string `"{}"`, the range is _[0, 2)_.
    pub fn new(start: usize, end: usize) -> Self {
        ArgumentSpec {
            range: (start, end),
            position: Position::default(),
            format: FormatType::default(),
            alternate: false,
            add_sign: false,
            pad_zero: false,
            fill_char: ' ',
            alignment: Alignment::default(),
            width: None,
            precision: None,
        }
    }

    /// Sets the argument position. Defaults to [`Position::Auto`].
    ///
    /// [`Position::Auto`]: enum.Position.html#variant.Auto
    pub fn with_position(mut self, position: Position<'a>) -> Self {
        self.position = position;
        self
    }

    /// Sets the formatting type. Defaults to [`FormatType::Display`].
    ///
    /// [`FormatType::Display`]: enum.FormatType.html#variant.Display
    pub fn with_format(mut self, format: FormatType) -> Self {
        self.format = format;
        self
    }

    /// Switch the formatter to [alternate] mode.
    ///
    /// [alternate]: https://doc.rust-lang.org/stable/std/fmt/struct.Formatter.html#method.alternate
    pub fn with_alternate(mut self, alternate: bool) -> Self {
        self.alternate = alternate;
        self
    }

    /// Always print a sign characters in front of numbers.
    pub fn with_sign(mut self, sign: bool) -> Self {
        self.add_sign = sign;
        self
    }

    /// Activate sign-aware zero padding.
    pub fn with_zeros(mut self, pad_zero: bool) -> Self {
        self.pad_zero = pad_zero;
        self
    }

    /// Set the fill character. Defaults to `' '` (a space).
    pub fn with_fill(mut self, fill_char: char) -> Self {
        self.fill_char = fill_char;
        self
    }

    /// Set alignment within the width of this format. Defaults to `Alignment::Right`.
    ///
    /// [`Alignment::Right`]: enum.Alignment.html#variant.Right
    pub fn with_alignment(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }

    /// Set a minimum width for this argument. Defaults to `None`.
    ///
    /// If the formatted argument is smaller than the threshold, the argument is padded with the
    /// fill character. If the argument is numeric and `with_zeros` is specified, it is padded with
    /// zeros instead.
    pub fn with_width(mut self, width: Option<Count<'a>>) -> Self {
        self.width = width;
        self
    }

    /// Set the precision for floating point values. Defaults to arbitrary precision.
    pub fn with_precision(mut self, precision: Option<Count<'a>>) -> Self {
        self.precision = precision;
        self
    }

    /// The start index of this specification in the format string.
    pub fn start(&self) -> usize {
        self.range.0
    }

    /// The end index of this specification in the format string.
    pub fn end(&self) -> usize {
        self.range.1
    }

    /// Executes the formatter for this argument and writes the result into the given writer.
    fn format_into<W, A>(&self, mut write: W, args: &mut ArgumentAccess<A>) -> Result<(), Error<'a>>
    where
        W: io::Write,
        A: FormatArgs,
    {
        if let FormatType::Literal(literal) = self.format {
            return write!(write, "{}", literal).map_err(Error::Io);
        }

        Formatter::new(write)
            .with_type(self.format)
            .with_alternate(self.alternate)
            .format(args.get_pos(self.position)?)
            .map_err(|e| Error::from_serialize(e, self.position))
    }
}

/// The result returned for each element of [`Format::iter_args`].
///
/// [`Format::iter_args`]: trait.Format.html#tymethod.iter_args
pub type ArgumentResult<'f> = Result<ArgumentSpec<'f>, Error<'f>>;

/// A format for string formatting.
///
/// This trait exposes formatting helper functions and lower-level utilities to interface with
/// format strings.
///
/// In its core, a format can parse a format string and return an iterator over [`ArgumentSpecs`].
/// Each specification refers to arguments in and [argument list] and contains information on how
/// to format it.
///
/// [`ArgumentSpecs`]: struct.ArgumentSpec.html
pub trait Format<'f> {
    /// The iterator returned by [`iter_args`].
    ///
    /// [`iter_args`]: trait.Format.html#tymethod.iter_args
    type Iter: Iterator<Item = ArgumentResult<'f>>;

    /// Returns an iterator over format arguments in the format string.
    ///
    /// This method is not meant to be used directly, instead use some of the provided methods to
    /// format a string.
    ///
    /// The iterator and this method are responsible for parsing the format string correctly and
    /// returning [`ArgumentSpecs`] for each argument in the format string. See the [module level]
    /// documentation for an example of how to implement this method.
    ///
    /// [`ArgumentSpecs`]: struct.ArgumentSpec.html
    /// [module level]: index.html#extensibility
    fn iter_args(&self, format: &'f str) -> Result<Self::Iter, Error<'f>>;

    /// Formats the given string with the specified arguments.
    ///
    /// Individual arguments must implement [`Debug`] and [`serde::Serialize`]. The arguments
    /// container must implement the [`FormatArgs`] trait.
    ///
    /// ```rust
    /// use dynfmt::{Format, NoopFormat};
    ///
    /// let formatted = NoopFormat.format("hello, world", &["unused"]);
    /// assert_eq!("hello, world", formatted.expect("formatting failed"));
    /// ```
    ///
    /// [`Debug`]: https://doc.rust-lang.org/stable/std/fmt/trait.Debug.html
    /// [`serde::Serialize`]: https://docs.rs/serde/latest/serde/trait.Serialize.html
    /// [`FormatArgs`]: trait.FormatArgs.html
    fn format<A>(&self, format: &'f str, arguments: A) -> Result<Cow<'f, str>, Error<'f>>
    where
        A: FormatArgs,
    {
        let mut iter = self.iter_args(format)?.peekable();
        if iter.peek().is_none() {
            return Ok(Cow::Borrowed(format));
        }

        let mut access = ArgumentAccess::new(arguments);
        let mut buffer = Vec::with_capacity(format.len());
        let mut last_match = 0;

        for spec in iter {
            let spec = spec?;
            buffer.extend(format[last_match..spec.start()].as_bytes());
            spec.format_into(&mut buffer, &mut access)?;
            last_match = spec.end();
        }

        buffer.extend(format[last_match..].as_bytes());
        Ok(Cow::Owned(unsafe { String::from_utf8_unchecked(buffer) }))
    }
}

/// A format implementation that does not format anything.
#[derive(Debug)]
pub struct NoopFormat;

impl<'f> Format<'f> for NoopFormat {
    type Iter = std::iter::Empty<ArgumentResult<'f>>;

    fn iter_args(&self, _format: &'f str) -> Result<Self::Iter, Error<'f>> {
        Ok(Default::default())
    }
}
