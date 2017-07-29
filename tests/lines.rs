use std::env;
use std::fs;
use std::io::Read;

extern crate difference;
extern crate ecl;
extern crate erased_serde;

use erased_serde::Serialize;

#[test]
fn test_lines() {
	for entry in fs::read_dir("tests/lines/").unwrap() {
		let path = entry.unwrap().path();
		if path.file_name().map_or(false, |f| f.to_string_lossy().starts_with("_")) { continue }
		if path.extension().unwrap() != "ecl" { continue }
		
		println!("Testing {:?}", path);
		
		let mut output = Vec::new();
		ecl::parse_file(&path.to_string_lossy())
			.erased_serialize(&mut ecl::lines::Serializer::new(&mut output)).unwrap();
		
		let mut lines = fs::File::open(path.with_extension("lines")).unwrap();
		let mut reference = Vec::new();
		lines.read_to_end(&mut reference).unwrap();
		
		if output != reference {
			let changes = difference::Changeset::new(
				&String::from_utf8(reference).unwrap(),
				&String::from_utf8(output).unwrap(),
				"\n");
			print!("{}", changes);
			env::remove_var("RUST_BACKTRACE");
			panic!("ERROR: Difference found in {:?}", path);
		}
	}
}
