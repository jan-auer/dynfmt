// #![warn(missing_docs)]

use std::borrow::Cow;
use std::fmt;
use std::io;

use erased_serde::Serialize as Serializable;
use serde::ser::Serialize;

mod formatter;

use crate::formatter::{FormatError, Formatter};

#[cfg(feature = "python")]
pub mod python;
#[cfg(feature = "python")]
pub use crate::python::PythonFormat;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Position<'a> {
    Auto,
    Index(usize),
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

#[derive(Debug)]
pub enum Error<'a> {
    BadFormat(char),
    ListRequired,
    MapRequired,
    MissingArg(Position<'a>),
    BadArg(Position<'a>, FormatType),
    BadData(Position<'a>, String),
    Io(io::Error),
}

impl<'a> Error<'a> {
    fn from_serialize(error: FormatError, position: Position<'a>) -> Self {
        match error {
            FormatError::Type(ty) => Error::BadArg(position, ty),
            FormatError::Serde(err) => Error::BadData(position, err),
            FormatError::Io(err) => Error::Io(err),
        }
    }
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::BadFormat(c) => write!(f, "unsupported format '{}'", c),
            Error::ListRequired => write!(f, "format requires an argument list"),
            Error::MapRequired => write!(f, "format requires an argument map"),
            Error::MissingArg(Position::Key(k)) => write!(f, "missing argument '{}'", k),
            Error::MissingArg(Position::Auto) | Error::MissingArg(Position::Index(_)) => {
                write!(f, "not enough format arguments")
            }
            Error::BadArg(pos, format) => {
                write!(f, "argument '{}' cannot be formatted as {}", pos, format)
            }
            Error::BadData(pos, reason) => {
                write!(f, "error formatting argument '{}': {}", pos, reason)
            }
            Error::Io(error) => write!(f, "{}", error),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FormatType {
    Display,
    Debug,
    Octal,
    LowerHex,
    UpperHex,
    Pointer,
    Binary,
    LowerExp,
    UpperExp,
    Literal(&'static str),
}

impl FormatType {
    pub fn name(self) -> &'static str {
        match self {
            FormatType::Display => "string",
            FormatType::Debug => "structure",
            FormatType::Octal => "octal",
            FormatType::LowerHex => "lower hex",
            FormatType::UpperHex => "upper hex",
            FormatType::Pointer => "pointer",
            FormatType::Binary => "binary",
            FormatType::LowerExp => "lower exp",
            FormatType::UpperExp => "upper exp",
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

pub type Argument<'a> = &'a dyn Serializable;

pub trait FormatArgs {
    #[allow(unused_variables)]
    fn get_index(&self, index: usize) -> Result<Option<Argument<'_>>, ()> {
        Err(())
    }

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

struct ArgumentAccess<A> {
    args: A,
    index: usize,
}

impl<A> ArgumentAccess<A>
where
    A: FormatArgs,
{
    pub fn new(args: A) -> Self {
        ArgumentAccess { args, index: 0 }
    }

    pub fn get_pos<'a>(&mut self, mut position: Position<'a>) -> Result<Argument<'_>, Error<'a>> {
        if position == Position::Auto {
            position = Position::Index(self.index);
            self.index += 1;
        }

        let result = match position {
            Position::Auto => unreachable!(),
            Position::Index(index) => self.args.get_index(index).map_err(|()| Error::ListRequired),
            Position::Key(key) => self.args.get_key(key).map_err(|()| Error::MapRequired),
        };

        result.and_then(|opt| opt.ok_or_else(|| Error::MissingArg(position)))
    }
}

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

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Count<'a> {
    Value(usize),
    Position(Position<'a>),
}

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
    /// The `start` and `end` parameters denote the inclusive range of this specification in the
    /// format string. E.g. for a format string `"{}"`, the range is `(0, 1)`.
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

    pub fn with_position(mut self, position: Position<'a>) -> Self {
        self.position = position;
        self
    }

    pub fn with_format(mut self, format: FormatType) -> Self {
        self.format = format;
        self
    }

    pub fn with_alternate(mut self, alternate: bool) -> Self {
        self.alternate = alternate;
        self
    }

    pub fn with_sign(mut self, sign: bool) -> Self {
        self.add_sign = sign;
        self
    }

    pub fn with_zeros(mut self, pad_zero: bool) -> Self {
        self.pad_zero = pad_zero;
        self
    }

    pub fn with_fill(mut self, fill_char: char) -> Self {
        self.fill_char = fill_char;
        self
    }

    pub fn with_alignment(mut self, alignment: Alignment) -> Self {
        self.alignment = alignment;
        self
    }

    pub fn with_width(mut self, width: Option<Count<'a>>) -> Self {
        self.width = width;
        self
    }

    pub fn with_precision(mut self, precision: Option<Count<'a>>) -> Self {
        self.precision = precision;
        self
    }

    pub fn start(&self) -> usize {
        self.range.0
    }

    pub fn end(&self) -> usize {
        self.range.1
    }

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

pub trait Format<'f> {
    type Iter: Iterator<Item = Result<ArgumentSpec<'f>, Error<'f>>>;

    fn iter_args(&self, format: &'f str) -> Result<Self::Iter, Error<'f>>;

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
