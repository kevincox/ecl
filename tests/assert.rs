extern crate ecl;

mod utils;

fn main() {
	utils::test_dir("assert", "ecl", |path| {
		let v = ecl::eval_file(&path.to_string_lossy()).eval();
		assert!(v.is_ok(), "Got error: {:?}", v);
	});
}
