extern crate difference;
extern crate ecl;
extern crate erased_serde;

mod utils;

use erased_serde::Serialize;

fn main() {
	utils::test_dir("lines", "ecl", |path| {
		let mut output = Vec::new();
		ecl::eval_file(&path.to_string_lossy())
			.erased_serialize(&mut ecl::lines::Serializer::new(&mut output)).unwrap();
		
		utils::diff(&output, &path.with_extension("lines"));
	})
}
