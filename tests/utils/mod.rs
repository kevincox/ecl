#![allow(unused)]

extern crate difference;
extern crate humanbool;
extern crate procinfo;

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
		.filter(move |p| p.extension().map_or(false, |e| e == ext)))
}

pub fn test_dir<
	F: std::panic::RefUnwindSafe + Fn(&std::path::Path) -> ()>
	(dir: &str, ext: &'static str, f: F)
{
	let mut tests = 0;
	let mut errors = 0;

	for path in scan_dir(dir, ext) {
		tests += 1;
		if let Err(_) = std::panic::catch_unwind(|| f(&path)) {
			errors += 1;
			println!("Error testing {:?}", path);
		}
	}

	if errors == 0 {
		eprintln!("{} tests completed successfully.", tests);
	} else {
		panic!("{}/{} tests failed.", errors, tests);
	}
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

pub fn read_or_empty(path: &std::path::Path) -> Vec<u8> {
	let mut f = match std::fs::File::open(path) {
		Ok(f) => f,
		Err(e) => {
			if e.kind() == std::io::ErrorKind::NotFound { return Vec::new() }
			panic!("reference file {:?} couldn't be opened: {:?}", path, e)
		}
	};

	let mut reference = Vec::new();
	f.read_to_end(&mut reference).unwrap();
	reference
}

pub fn diff(expected: &[u8], path: &std::path::Path) {
	let reference = read_or_empty(path);

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

// pub fn check_leaks
