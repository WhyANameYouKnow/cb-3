use cb_3::parser::C1Parser;
use std::fs;

#[test]
fn run_example() {
    let text = fs::read_to_string("tests/data/beispiel.c-1").unwrap();
    let result = C1Parser::parse(text.as_str());
    assert!(result.is_ok(), "Parse result: {}", result.err().unwrap());
}
