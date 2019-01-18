#![cfg(feature = "curly")]

use dynfmt::{Format, SimpleCurlyFormat};

macro_rules! test_fmt {
    ($name:ident, $expected:expr, $format:expr, $($args:expr),* $(,)*) => {
        #[test]
        fn $name() {
            assert_eq!(
                $expected,
                SimpleCurlyFormat
                    .format($format, &[$($args),*])
                    .expect("formatting failed")
            );
        }
    };
}

test_fmt!(string_display, "hello, world!", "hello, {}!", "world");
test_fmt!(number_display, "hello, 42!", "hello, {}!", 42);
test_fmt!(negative_display, "hello, -42!", "hello, {}!", -42);
test_fmt!(float_display, "hello, 4.2!", "hello, {}!", 4.2);
test_fmt!(boolean_display, "hello, true!", "hello, {}!", true);

test_fmt!(string_display_by_index, "hello, world!", "hello, {0}!", "world");

#[test]
fn string_display_by_name() {
    let mut args = std::collections::BTreeMap::new();
    args.insert("name", "world");

    assert_eq!(
        "hello, world!",
        SimpleCurlyFormat.format("hello, {name}!", args).expect("formatting failed")
    );
}
