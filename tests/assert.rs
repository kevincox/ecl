extern crate ecl;

mod utils;

fn main() {
	utils::test_dir("assert", "ecl", |path| {
		let v = ecl::eval_file(&path.to_string_lossy());
		assert!(!v.is_err(), "Got error: {:?}", v);
	});
}
