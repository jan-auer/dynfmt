#![cfg(feature = "python")]

use dynfmt::{Error, Format, PythonFormat};

#[test]
fn writes_formatted_output() {
    let mut buffer = Vec::new();
    PythonFormat
        .format_into(&mut buffer, "hello, %s!", ["world"])
        .expect("formatting failed");
    assert_eq!(b"hello, world!", buffer.as_slice());
}

#[test]
fn aborts_on_full_writer() {
    let mut buffer = [0u8; 8];
    let result = PythonFormat.format_into(buffer.as_mut_slice(), "hello, %s!", ["world"]);
    assert!(matches!(result.unwrap_err(), Error::BadData(..)));
}
