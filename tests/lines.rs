use std::env;
use std::fs;
use std::io::Read;

extern crate difference;
extern crate ecl;
extern crate serde;

use serde::Serialize;

#[test]
fn test_lines() {
	let mut good = 0;
	let mut bad = 0;
	
	for entry in fs::read_dir("tests/lines/").unwrap() {
		let path = entry.unwrap().path();
		if path.extension().unwrap() != "ecl" { continue }
		
		// println!("Testing {:?}", path);
		
		let mut ecl = fs::File::open(path.as_path()).unwrap();
		let mut lines = fs::File::open(path.with_extension("lines")).unwrap();
		
		let mut input = String::new();
		ecl.read_to_string(&mut input).unwrap();
		let mut output = String::new();
		ecl::parse(&input).unwrap()
			.serialize(&mut ecl::lines::Serializer::new(&mut output)).unwrap();
		
		input.clear();
		lines.read_to_string(&mut input).unwrap();
		
		if input == output {
			good += 1;
		} else {
			bad += 1;
			difference::print_diff(&input, &output, "\n");
			println!("ERROR: Above difference found in {:?}", path);
		}
	}
	
	println!("RESULT: {}/{} differ from the expected value.", bad, bad + good);
	
	env::remove_var("RUST_BACKTRACE");
	assert_eq!(bad, 0);
	assert!(good > 0);
}
