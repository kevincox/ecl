extern crate difference;
extern crate humanbool;

use std;
use std::io::Read;
use std::io::Write;

fn regen_regression_tests() -> bool {
	humanbool::env("ECL_REGEN", "n").unwrap()
}

pub fn scan_dir(dir: &str, ext: &'static str)
	-> Box<Iterator<Item=std::path::PathBuf>>
{
	Box::new(std::fs::read_dir(format!("tests/{}", dir)).unwrap()
		.map(|f| f.unwrap().path())
		.filter(|p| p.file_name().map_or(false, |n| !n.to_string_lossy().starts_with('_')))
		.filter(move |p| p.extension().map_or(false, |e| e == ext))
		.inspect(|p| println!("Testing {:?}", p)))
}

fn regen(expected: &[u8], path: &std::path::Path) -> bool {
	if !regen_regression_tests() {
		return false
	}
	
	println!("Regenerating {:?}", path);
	let mut f = std::fs::File::create(path)
		.expect("reference file couldn't be opened for regen.");
	f.write_all(expected).unwrap();
	
	return true
}

pub fn diff(expected: &[u8], path: &std::path::Path) {
	let mut f = match std::fs::File::open(path) {
		Ok(f) => f,
		Err(e) => {
			if e.kind() == std::io::ErrorKind::NotFound && expected.is_empty() { return }
			if regen(expected, path) { return }
			panic!("reference file {:?} couldn't be opened: {:?}", path, e)
		}
	};

	let mut reference = Vec::new();
	f.read_to_end(&mut reference).unwrap();
	
	if expected != reference.as_slice() {
		if regen(expected, path) { return }
		
		let changes = difference::Changeset::new(
			&String::from_utf8_lossy(&reference),
			&String::from_utf8_lossy(expected),
			"\n");
		print!("{}", changes);
		std::env::remove_var("RUST_BACKTRACE");
		panic!("ERROR: Difference found in {:?}", path);
	}
}
