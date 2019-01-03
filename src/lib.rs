// #![warn(missing_docs)]

use std::borrow::Cow;
use std::fmt::{self, Write};
use std::str::FromStr;

use erased_serde::Serialize;
use regex::{Captures, Regex};

lazy_static::lazy_static! {
    static ref OLD_STYLE_RE: Regex = Regex::new(r"(?x)
        %
        (?P<key>\(\w+\))?                            # Mapping key
        (?P<flags>[\#|0|\-| |+]*)?                   # Conversion flags
        (?P<width>\*|\d+)?                           # Minimum field width
        (?:.(?P<precision>\*|\d+))?                  # Precision after decimal point
        [h|l|L]*                                     # Ignored length modifier
        (?P<type>[d|i|o|u|x|X|e|E|f|F|g|G|c|r|s|%])  # Conversion type
    ").unwrap();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Ord, PartialOrd, Hash)]
pub enum Position<'a> {
    Index(usize),
    Key(&'a str),
}

impl fmt::Display for Position<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
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
    BadArg(Position<'a>, &'static str),
    BadRepr(Position<'a>, String),
    Fmt(std::fmt::Error),
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::BadFormat(c) => write!(f, "unsupported format '{}'", c),
            Error::ListRequired => write!(f, "format requires an argument list"),
            Error::MapRequired => write!(f, "format requires an argument map"),
            Error::MissingArg(Position::Index(_)) => write!(f, "not enough format arguments"),
            Error::MissingArg(Position::Key(k)) => write!(f, "missing argument '{}'", k),
            Error::BadArg(pos, format) => {
                write!(f, "argument '{}' cannot be formatted as {}", pos, format)
            }
            Error::BadRepr(pos, reason) => write!(f, "error representing '{}': {}", pos, reason),
            Error::Fmt(error) => write!(f, "{}", error),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum FormatMode {
    NewStyle,
    OldStyle,
}

impl Default for FormatMode {
    fn default() -> Self {
        FormatMode::NewStyle
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum ReprMode {
    Debug,
    Json,
}

impl Default for ReprMode {
    fn default() -> Self {
        ReprMode::Debug
    }
}

pub trait FormatArg {
    fn as_display(&self) -> Option<&dyn std::fmt::Display> {
        None
    }

    fn as_debug(&self) -> Option<&dyn std::fmt::Debug> {
        None
    }

    fn as_octal(&self) -> Option<&dyn std::fmt::Octal> {
        None
    }

    fn as_lower_hex(&self) -> Option<&dyn std::fmt::LowerHex> {
        None
    }

    fn as_upper_hex(&self) -> Option<&dyn std::fmt::UpperHex> {
        None
    }

    fn as_lower_exp(&self) -> Option<&dyn std::fmt::LowerExp> {
        None
    }

    fn as_upper_exp(&self) -> Option<&dyn std::fmt::UpperExp> {
        None
    }

    fn as_serialize(&self) -> Option<&dyn Serialize> {
        None
    }
}

macro_rules! derive_format_method {
    (display) => {
        fn as_display(&self) -> Option<&dyn std::fmt::Display> { Some(self) }
    };
    (debug) => {
        fn as_debug(&self) -> Option<&dyn std::fmt::Debug> { Some(self) }
    };
    (octal) => {
        fn as_octal(&self) -> Option<&dyn std::fmt::Octal> { Some(self) }
    };
    (lower_hex) => {
        fn as_lower_hex(&self) -> Option<&dyn std::fmt::LowerHex> { Some(self) }
    };
    (upper_hex) => {
        fn as_upper_hex(&self) -> Option<&dyn std::fmt::UpperHex> { Some(self) }
    };
    (lower_exp) => {
        fn as_lower_exp(&self) -> Option<&dyn std::fmt::LowerExp> { Some(self) }
    };
    (upper_exp) => {
        fn as_upper_exp(&self) -> Option<&dyn std::fmt::UpperExp> { Some(self) }
    };
    (serialize) => {
        fn as_serialize(&self) -> Option<&dyn Serialize> { Some(self) }
    };
    ($arg:ident,) => {
        derive_format_method!($arg);
    };
    ($arg:ident, $($args:tt)*) => {
        derive_format_method!($arg);
        derive_format_method!($($args)*);
    };
}

macro_rules! derive_format_arg {
    ($type:tt, $($args:tt)*) => {
        impl FormatArg for $type {
            derive_format_method!($($args)*);
        }
    };
    (&'_ $type:tt, $($args:tt)*) => {
        impl FormatArg for &'_ $type {
            derive_format_method!($($args)*);
        }
    };
}

derive_format_arg!(String, display, debug, serialize);
derive_format_arg!(&'_ str, display, debug, serialize);
derive_format_arg!(bool, display, debug, serialize);
derive_format_arg!(char, display, debug, serialize);
derive_format_arg!(u8, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(i8, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(u16, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(i16, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(u32, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(i32, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(u64, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(i64, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(u128, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(i128, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(usize, display, debug, octal, lower_hex, upper_hex, serialize);
derive_format_arg!(f32, display, debug, lower_exp, upper_exp, serialize);
derive_format_arg!(f64, display, debug, lower_exp, upper_exp, serialize);

pub trait FormatArgs {
    #[allow(unused_variables)]
    fn get_index(&self, index: usize) -> Result<Option<&dyn FormatArg>, ()> {
        Err(())
    }

    #[allow(unused_variables)]
    fn get_key(&self, key: &str) -> Result<Option<&dyn FormatArg>, ()> {
        Err(())
    }

    fn get_pos(&self, position: Position<'_>) -> Result<Option<&dyn FormatArg>, Error<'static>> {
        match position {
            Position::Index(index) => self.get_index(index).map_err(|()| Error::ListRequired),
            Position::Key(key) => self.get_key(key).map_err(|()| Error::MapRequired),
        }
    }
}

impl<T> FormatArgs for Vec<T>
where
    T: FormatArg,
{
    fn get_index(&self, index: usize) -> Result<Option<&dyn FormatArg>, ()> {
        Ok(self.get(index).map(|arg| arg as &dyn FormatArg))
    }
}

impl<T> FormatArgs for &'_ [T]
where
    T: FormatArg,
{
    fn get_index(&self, index: usize) -> Result<Option<&dyn FormatArg>, ()> {
        Ok(self.get(index).map(|arg| arg as &dyn FormatArg))
    }
}

impl<T> FormatArgs for std::collections::VecDeque<T>
where
    T: FormatArg,
{
    fn get_index(&self, index: usize) -> Result<Option<&dyn FormatArg>, ()> {
        Ok(self.get(index).map(|arg| arg as &dyn FormatArg))
    }
}

impl<S, T> FormatArgs for std::collections::BTreeMap<S, T>
where
    S: std::borrow::Borrow<str> + Ord,
    T: FormatArg,
{
    fn get_key(&self, key: &str) -> Result<Option<&dyn FormatArg>, ()> {
        Ok(self.get(key).map(|arg| arg as &dyn FormatArg))
    }
}

impl<S, T> FormatArgs for std::collections::HashMap<S, T>
where
    S: std::borrow::Borrow<str> + std::hash::Hash + Eq,
    T: FormatArg,
{
    fn get_key(&self, key: &str) -> Result<Option<&dyn FormatArg>, ()> {
        Ok(self.get(key).map(|arg| arg as &dyn FormatArg))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FormatType {
    /// Signed integer decimal.
    Int,
    /// Signed octal value.
    Octal,
    /// Signed hexadecimal (lowercase).
    LowerHex,
    /// Signed hexadecimal (lowercase).
    UpperHex,
    /// Floating point exponential format (lowercase).
    LowerExp,
    /// Floating point exponential format (lowercase).
    UpperExp,
    /// Floating point decimal format.
    Float,
    /// Floating point format. Uses lowercase exponential format if exponent is less than -4 or not
    /// less than precision, decimal format otherwise.
    LowerFloat,
    /// Floating point format. Uses uppercase exponential format if exponent is less than -4 or not
    /// less than precision, decimal format otherwise.
    UpperFloat,
    /// Single character (accepts integer or single character string).
    Char,
    /// String (serializes objects based on the `ReprMode`).
    Repr,
    /// String (stringifies objects using the `Display` trait).
    Str,
    /// No argument, results in a `%` character.
    Percent,
}

impl FromStr for FormatType {
    type Err = Error<'static>;

    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ok(match string {
            "d" | "i" | "u" => FormatType::Int,
            "o" => FormatType::Octal,
            "x" => FormatType::LowerHex,
            "X" => FormatType::UpperHex,
            "e" => FormatType::LowerExp,
            "E" => FormatType::UpperExp,
            "f" | "F" => FormatType::Float,
            "g" => FormatType::LowerFloat,
            "G" => FormatType::UpperFloat,
            "c" => FormatType::Char,
            "r" => FormatType::Repr,
            "s" => FormatType::Str,
            "%" => FormatType::Percent,
            s => return Err(Error::BadFormat(s.chars().next().unwrap_or_default())),
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct FormatSpec<'a> {
    /// The position for argument lookups.
    pub position: Position<'a>,
    /// Flag: Display alternate form.
    pub alternate: bool,
    /// Flag: Pad with zeros for numeric values.
    pub pad_zero: bool,
    /// Flag: Adjust value to the left (overrides `pad_zero`).
    pub adjust_left: bool,
    /// Flag: Add a blank before the sign (requires `sign`).
    pub blank: bool,
    /// Flag: Precede with a sign character.
    pub sign: bool,
    /// Minimum field width.
    // TODO(ja): Asterisk
    pub width: Option<usize>,
    /// Floating point precision.
    // TODO(ja): Asterisk
    pub precision: Option<usize>,
    /// The conversion type.
    pub ty: FormatType,
}

impl<'a> FormatSpec<'a> {
    pub fn parse(index: usize, captures: &Captures<'a>) -> Result<Self, Error<'a>> {
        let mut spec = FormatSpec {
            position: captures
                .name("key")
                .map(|m| Position::Key(m.as_str()))
                .unwrap_or_else(|| Position::Index(index)),
            alternate: false,
            pad_zero: false,
            adjust_left: false,
            blank: false,
            sign: false,
            width: captures.name("width").and_then(|m| m.as_str().parse().ok()),
            precision: captures
                .name("precision")
                .and_then(|m| m.as_str().parse().ok()),
            ty: captures["type"].parse()?,
        };

        if let Some(flags) = captures.name("flags") {
            for flag in flags.as_str().chars() {
                match flag {
                    '#' => spec.alternate = true,
                    '0' => spec.pad_zero = true,
                    '-' => spec.adjust_left = true,
                    ' ' => spec.blank = true,
                    '+' => spec.sign = true,
                    _ => unreachable!(),
                }
            }
        }

        Ok(spec)
    }

    pub fn pyfmt(
        self,
        pos: Position<'a>,
        argument: &dyn FormatArg,
        target: &mut String,
        options: &FormatOptions,
    ) -> Result<(), Error<'a>> {
        match self.ty {
            FormatType::Int
            | FormatType::LowerFloat
            | FormatType::UpperFloat
            | FormatType::Float
            | FormatType::Char
            | FormatType::Str => {
                let arg = argument
                    .as_display()
                    .ok_or_else(|| Error::BadArg(pos, "string"))?;
                write!(target, "{}", arg).map_err(Error::Fmt)?;
            }
            FormatType::Octal => {
                let arg = argument
                    .as_octal()
                    .ok_or_else(|| Error::BadArg(pos, "octal"))?;
                write!(target, "{:o}", arg).map_err(Error::Fmt)?;
            }
            FormatType::LowerHex => {
                let arg = argument
                    .as_lower_hex()
                    .ok_or_else(|| Error::BadArg(pos, "hex"))?;
                write!(target, "{:x}", arg).map_err(Error::Fmt)?;
            }
            FormatType::UpperHex => {
                let arg = argument
                    .as_upper_hex()
                    .ok_or_else(|| Error::BadArg(pos, "hex"))?;
                write!(target, "{:X}", arg).map_err(Error::Fmt)?;
            }
            FormatType::LowerExp => {
                let arg = argument
                    .as_lower_exp()
                    .ok_or_else(|| Error::BadArg(pos, "exp"))?;
                write!(target, "{:e}", arg).map_err(Error::Fmt)?;
            }
            FormatType::UpperExp => {
                let arg = argument
                    .as_upper_exp()
                    .ok_or_else(|| Error::BadArg(pos, "exp"))?;
                write!(target, "{:E}", arg).map_err(Error::Fmt)?;
            }
            FormatType::Repr => match options.repr {
                ReprMode::Debug => {
                    let arg = argument
                        .as_debug()
                        .ok_or_else(|| Error::BadArg(pos, "debug"))?;
                    write!(target, "{:?}", arg).map_err(Error::Fmt)?;
                }
                ReprMode::Json => {
                    let arg = argument
                        .as_serialize()
                        .ok_or_else(|| Error::BadArg(pos, "json"))?;
                    let json = serde_json::to_string(arg)
                        .map_err(|e| Error::BadRepr(pos, e.to_string()))?;
                    target.push_str(&json);
                }
            },
            FormatType::Percent => {
                write!(target, "%").ok();
            }
        }

        Ok(())
    }
}

#[derive(Clone)]
pub struct FormatOptions {
    pub mode: FormatMode,
    pub repr: ReprMode,
    pub simple: bool,
}

impl FormatOptions {
    pub fn new_style() -> Self {
        FormatOptions::with_mode(FormatMode::NewStyle)
    }

    pub fn old_style() -> Self {
        FormatOptions::with_mode(FormatMode::OldStyle)
    }

    pub fn pyfmt<'f, A>(&self, format: &'f str, args: &A) -> Result<Cow<'f, str>, Error<'f>>
    where
        A: FormatArgs,
    {
        let mut iter = OLD_STYLE_RE.captures_iter(format).enumerate().peekable();
        if iter.peek().is_none() {
            return Ok(Cow::Borrowed(format));
        }

        let mut string = String::with_capacity(format.len());
        let mut last_match = 0;

        for (index, captures) in iter {
            let mat = captures.get(0).unwrap();
            string.push_str(&format[last_match..mat.start()]);

            let spec = FormatSpec::parse(index, &captures)?;
            let argument = args
                .get_pos(spec.position)?
                .ok_or_else(|| Error::MissingArg(spec.position))?;

            spec.pyfmt(spec.position, argument, &mut string, self)?;
            last_match = mat.end();
        }

        string.push_str(&format[last_match..]);
        Ok(Cow::Owned(string))
    }

    fn with_mode(mode: FormatMode) -> Self {
        FormatOptions {
            mode,
            repr: ReprMode::Debug,
            simple: false,
        }
    }
}

impl Default for FormatOptions {
    fn default() -> Self {
        FormatOptions::new_style()
    }
}

pub fn pyfmt<'f, A>(format: &'f str, args: &A) -> Result<Cow<'f, str>, Error<'f>>
where
    A: FormatArgs,
{
    FormatOptions::new_style().pyfmt(format, args)
}

pub fn pyfmt_old<'f, A>(format: &'f str, args: &A) -> Result<Cow<'f, str>, Error<'f>>
where
    A: FormatArgs,
{
    FormatOptions::old_style().pyfmt(format, args)
}
