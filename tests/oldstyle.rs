use pyfmt::{pyfmt_old, FormatMode, FormatOptions, ReprMode};

#[test]
fn test_no_format() {
    assert_eq!(
        "hello world",
        pyfmt_old("hello world", &Vec::<String>::new()).expect("pyfmt errored")
    );
}

#[test]
fn test_basic_string() {
    assert_eq!(
        "hello world",
        pyfmt_old("hello %s", &vec!["world".to_string()]).expect("pyfmt errored")
    );
}

#[test]
fn test_display_string() {
    assert_eq!(
        "hello \"world\"",
        pyfmt_old("hello %r", &vec!["world".to_string()]).expect("pyfmt errored")
    );
}

#[test]
fn test_json_string() {
    let opts = FormatOptions {
        mode: FormatMode::OldStyle,
        repr: ReprMode::Json,
        simple: false,
    };
    assert_eq!(
        "hello \"world\"",
        opts.pyfmt("hello %r", &vec!["world".to_string()])
            .expect("pyfmt errored")
    );
}
