extern crate difference;
extern crate ecl;
extern crate erased_serde;

mod utils;

use erased_serde::Serialize;

#[test]
fn test_lines() {
	for path in utils::scan_dir("lines", "ecl") {
		let mut output = Vec::new();
		ecl::eval_file(&path.to_string_lossy())
			.erased_serialize(&mut ecl::lines::Serializer::new(&mut output)).unwrap();
		
		utils::diff(&output, &path.with_extension("lines"));
	}
}
