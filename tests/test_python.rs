#![cfg(feature = "python")]

use std::collections::BTreeMap;

use dynfmt::{Format, PythonFormat};

macro_rules! test_fmt {
    ($name:ident, $expected:expr, $format:expr, $($args:expr),* $(,)*) => {
        #[test]
        fn $name() {
            assert_eq!(
                $expected,
                PythonFormat
                    .format($format, &[$($args),*])
                    .expect("formatting failed")
            );
        }
    };
}

test_fmt!(string_display, "hello, world!", "hello, %s!", "world");
test_fmt!(number_display, "hello, 42!", "hello, %s!", 42);
test_fmt!(negative_display, "hello, -42!", "hello, %s!", -42);
test_fmt!(float_display, "hello, 4.2!", "hello, %s!", 4.2);
test_fmt!(boolean_display, "hello, true!", "hello, %s!", true);
test_fmt!(array_display, "hello, [1,2,3]!", "hello, %s!", [1, 2, 3]);
test_fmt!(object_display, "hello, {\"foo\":\"bar\"}!", "hello, %s!", {
    let mut map = BTreeMap::new();
    map.insert("foo", "bar");
    map
});

test_fmt!(string_repr, "hello, \"world\"!", "hello, %r!", "world");
test_fmt!(array_repr, "hello, [1,2,3]!", "hello, %r!", [1, 2, 3]);
test_fmt!(
    array_repr_alt,
    "hello, [\n  1,\n  2,\n  3\n]!",
    "hello, %#r!",
    [1, 2, 3],
);
test_fmt!(object_repr, "hello, {\"foo\":\"bar\"}!", "hello, %r!", {
    let mut map = BTreeMap::new();
    map.insert("foo", "bar");
    map
});

test_fmt!(number_octal, "hello, 52!", "hello, %o!", 42);
test_fmt!(number_octal_alt, "hello, 0o52!", "hello, %#o!", 42);
test_fmt!(number_lower_hex, "hello, 2a!", "hello, %x!", 42);
test_fmt!(number_lower_hex_alt, "hello, 0x2a!", "hello, %#x!", 42);
test_fmt!(number_upper_hex, "hello, 2A!", "hello, %X!", 42);
test_fmt!(number_upper_hex_alt, "hello, 0x2A!", "hello, %#X!", 42);

test_fmt!(float_lower_exp, "hello, 4.2e0!", "hello, %e!", 4.2);
test_fmt!(float_upper_exp, "hello, 4.2E0!", "hello, %E!", 4.2);

#[test]
fn string_display_by_name() {
    let mut args = std::collections::BTreeMap::new();
    args.insert("name", "world");

    assert_eq!(
        "hello, world!",
        PythonFormat
            .format("hello, %(name)s!", args)
            .expect("formatting failed")
    );
}
