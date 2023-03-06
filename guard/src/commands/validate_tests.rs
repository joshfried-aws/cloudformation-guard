use super::super::*;
use super::*;
use crate::utils::reader::ReadBuffer;
use crate::utils::reader::ReadBuffer::Stdin;

#[test]
fn test_deserialize_payload_success() {
    let serialized_payload = "{\"data\":[\"data as string 1\",\"data as string 2\"], \"rules\":[\"rules as string 1\",\"rules as string 2\"]}";
    let deserialized_payload = deserialize_payload(serialized_payload).unwrap();
    assert_eq!(
        deserialized_payload.list_of_data,
        vec!["data as string 1", "data as string 2"]
    );
    assert_eq!(
        deserialized_payload.list_of_rules,
        vec!["rules as string 1", "rules as string 2"]
    );
}

#[test]
#[should_panic]
fn test_deserialize_payload_malformed_string() {
    let serialized_payload = "{\"data:[\"data as string 1\",\"data as string 2\"], \"rules\":[\"rules as string 1\",\"rules as string 2\"]}";
    deserialize_payload(serialized_payload).unwrap();
}

#[test]
#[should_panic]
fn test_deserialize_payload_unrecognized_property() {
    let serialized_payload = "{\"data\":[\"data as string 1\",\"data as string 2\"], \"wrongProperty\":\"wrongProperty\"}";
    deserialize_payload(serialized_payload).unwrap();
}

#[test]
#[should_panic]
fn test_deserialize_payload_empty_input() {
    let serialized_payload = "";
    deserialize_payload(serialized_payload).unwrap();
}

#[test]
fn test_handle_data_files_as_arg() -> Result<()> {
    let path = String::from("resources/validate/data-dir");
    let cmd = Validate2 {
        data: vec![path],
        ..Default::default()
    };

    let mut reader = Reader::new(Stdin(std::io::stdin()));

    let data_files = cmd.handle_data_files(&mut reader)?;

    assert_eq!(6, data_files.len());

    Ok(())
}

#[test]
fn test_handle_data_files_from_stdin() -> Result<()> {
    let cmd = Validate2 {
        ..Default::default()
    };

    let mut reader = Reader::new(ReadBuffer::File(
        File::open("resources/validate/data-dir/s3-public-read-prohibited-template-compliant.yaml")
            .expect("invalid path"),
    ));

    let data_files = cmd.handle_data_files(&mut reader)?;

    assert_eq!(1, data_files.len());

    Ok(())
}

#[test]
fn test_supported_extensions() {
    // Data extensions
    assert!(has_a_supported_extension(
        "blah.json",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(has_a_supported_extension(
        "blah.jsn",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(has_a_supported_extension(
        "blah.yaml",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(has_a_supported_extension(
        "blah.template",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(has_a_supported_extension(
        "blah.yml",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    // unsupported
    assert!(!has_a_supported_extension(
        "blah.txt",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(!has_a_supported_extension(
        "blah",
        &DATA_FILE_SUPPORTED_EXTENSIONS
    ));

    // Rules extensions
    assert!(has_a_supported_extension(
        "blah.guard",
        &RULE_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(has_a_supported_extension(
        "blah.ruleset",
        &RULE_FILE_SUPPORTED_EXTENSIONS
    ));
    // unsupported
    assert!(!has_a_supported_extension(
        "blah.txt",
        &RULE_FILE_SUPPORTED_EXTENSIONS
    ));
    assert!(!has_a_supported_extension(
        "blah",
        &RULE_FILE_SUPPORTED_EXTENSIONS
    ));
}
