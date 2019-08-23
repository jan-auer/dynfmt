use std::borrow::Cow;

use dynfmt::{Error, FormatType, Position};

macro_rules! test_fmt {
    ($name:ident, $expected:expr, $error:expr) => {
        #[test]
        fn $name() {
            assert_eq!($expected, format!("{}", $error));
        }
    };
}

test_fmt!(bad_format, "unsupported format 'x'", Error::BadFormat('x'));
test_fmt!(
    parse,
    "error parsing format string: x",
    Error::Parse(Cow::Borrowed("x"))
);
test_fmt!(
    list_required,
    "format requires an argument list",
    Error::ListRequired
);
test_fmt!(
    map_required,
    "format requires an argument map",
    Error::MapRequired
);
test_fmt!(
    missing_arg_key,
    "missing argument: x",
    Error::MissingArg(Position::Key("x"))
);
test_fmt!(
    missing_arg_auto,
    "missing argument: {next}",
    Error::MissingArg(Position::Auto)
);
test_fmt!(
    missing_arg_index,
    "missing argument: 42",
    Error::MissingArg(Position::Index(42))
);
test_fmt!(
    bad_arg_key,
    "argument 'x' cannot be formatted as object",
    Error::BadArg(Position::Key("x"), FormatType::Object)
);
test_fmt!(
    bad_arg_auto,
    "argument '{next}' cannot be formatted as object",
    Error::BadArg(Position::Auto, FormatType::Object)
);
test_fmt!(
    bad_arg_index,
    "argument '42' cannot be formatted as object",
    Error::BadArg(Position::Index(42), FormatType::Object)
);
test_fmt!(
    bad_data_key,
    "error formatting argument 'x': %x",
    Error::BadData(Position::Key("x"), "%x".into())
);
test_fmt!(
    bad_data_auto,
    "error formatting argument '{next}': %x",
    Error::BadData(Position::Auto, "%x".into())
);
test_fmt!(
    bad_data_index,
    "error formatting argument '42': %x",
    Error::BadData(Position::Index(42), "%x".into())
);
test_fmt!(
    io_error,
    "oops",
    Error::Io(std::io::Error::new(std::io::ErrorKind::Other, "oops"))
);
