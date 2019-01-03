//! TODO(ja): Doc

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

#[derive(Debug)]
pub enum Error {
    InvalidChar(char),
    ListRequired,
    MapRequired,
    MissingIndex,
    MissingKey,
    InvalidArg,
    JsonFailed,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::InvalidChar(c) => write!(f, "unsupported format character '{}'", c),
            Error::ListRequired => write!(f, "format requires an argument list"),
            Error::MapRequired => write!(f, "format requires an argument map"),
            Error::MissingIndex => write!(f, "not enough format arguments"),
            Error::MissingKey => write!(f, "missing named format argument"),
            Error::InvalidArg => write!(f, "invalid format argument"),
            Error::JsonFailed => write!(f, "could not serialize to JSON"),
        }
    }
}

/// TODO(ja): Doc
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum FormatMode {
    /// TODO(ja): Doc
    NewStyle,
    /// TODO(ja): Doc
    OldStyle,
}

impl Default for FormatMode {
    fn default() -> Self {
        FormatMode::NewStyle
    }
}

/// TODO(ja): Doc
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum ReprMode {
    /// TODO(ja): Doc
    Debug,
    /// TODO(ja): Doc
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
    type Err = Error;

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
            s => return Err(Error::InvalidChar(s.chars().next().unwrap_or_default())),
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct FormatSpec<'a> {
    /// The key for named lookups.
    pub key: Option<&'a str>,
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
    /// TODO(ja): Asterisk
    pub width: Option<usize>,
    /// Floating point precision.
    /// TODO(ja): Asterisk
    pub precision: Option<usize>,
    /// The conversion type.
    pub ty: FormatType,
}

impl<'a> FormatSpec<'a> {
    pub fn parse(captures: &'a Captures) -> Result<Self, Error> {
        let mut spec = FormatSpec {
            key: captures.name("key").map(|m| m.as_str()),
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

    #[rustfmt::skip]
    pub fn pyfmt(
        self,
        argument: &dyn FormatArg,
        target: &mut String,
        options: &FormatOptions,
    ) -> Result<(), Error> {
        match self.ty {
            FormatType::Int
            | FormatType::LowerFloat
            | FormatType::UpperFloat
            | FormatType::Float
            | FormatType::Char
            | FormatType::Str => {
                write!(target, "{}", argument.as_display().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::Octal => {
                write!(target, "{:o}", argument.as_octal().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::LowerHex => {
                write!(target, "{:x}", argument.as_lower_hex().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::UpperHex => {
                write!(target, "{:X}", argument.as_upper_hex().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::LowerExp => {
                write!(target, "{:e}", argument.as_lower_exp().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::UpperExp => {
                write!(target, "{:E}", argument.as_upper_exp().ok_or(Error::InvalidArg)?).ok();
            }
            FormatType::Repr => match options.repr {
                ReprMode::Debug => {
                    write!(target, "{:?}", argument.as_debug().ok_or(Error::InvalidArg)?).ok();
                }
                ReprMode::Json => {
                    let json = serde_json::to_string(argument.as_serialize().ok_or(Error::InvalidArg)?)
                        .map_err(|_| Error::JsonFailed)?;
                    target.push_str(&json);
                }
            },
            FormatType::Percent => {
                write!(target, "%").ok();
            },
        }

        Ok(())
    }
}

/// TODO(ja): Doc
#[derive(Clone)]
pub struct FormatOptions {
    /// TODO(ja): Doc
    pub mode: FormatMode,
    /// TODO(ja): Doc
    pub repr: ReprMode,
    /// TODO(ja): Doc
    pub simple: bool,
}

impl FormatOptions {
    /// TODO(ja): Doc
    pub fn new_style() -> Self {
        FormatOptions::with_mode(FormatMode::NewStyle)
    }

    /// TODO(ja): Doc
    pub fn old_style() -> Self {
        FormatOptions::with_mode(FormatMode::OldStyle)
    }

    /// TODO(ja): Doc
    pub fn pyfmt<'f, A>(&self, format: &'f str, args: &A) -> Result<Cow<'f, str>, Error>
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

            let spec = FormatSpec::parse(&captures)?;
            let argument = match spec.key {
                Some(ref key) => args
                    .get_key(key)
                    .map_err(|_| Error::MapRequired)?
                    .ok_or(Error::MissingKey)?,
                None => args
                    .get_index(index)
                    .map_err(|_| Error::ListRequired)?
                    .ok_or(Error::MissingIndex)?,
            };

            spec.pyfmt(argument, &mut string, self)?;
            last_match = mat.end();
        }

        string.push_str(&format[last_match..]);
        Ok(Cow::Owned(string))
    }

    /// TODO(ja): Doc
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

/// TODO(ja): Doc
pub fn pyfmt<'f, A>(format: &'f str, args: &A) -> Result<Cow<'f, str>, Error>
where
    A: FormatArgs,
{
    FormatOptions::new_style().pyfmt(format, args)
}

/// TODO(ja): Doc
pub fn pyfmt_old<'f, A>(format: &'f str, args: &A) -> Result<Cow<'f, str>, Error>
where
    A: FormatArgs,
{
    FormatOptions::old_style().pyfmt(format, args)
}
