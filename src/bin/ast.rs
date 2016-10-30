use std::env;
use std::fs;
use std::io::Read;

extern crate ecl;
extern crate serde;

fn main() {
	let args: Vec<String> = env::args().collect();
	for path in &args[1..] {
		println!("Path {}", path);
		let mut ecl = fs::File::open(path).unwrap();
		
		let mut buf = String::new();
		ecl.read_to_string(&mut buf).unwrap();
		
		ecl::dump_ast(&buf).unwrap();
	}
}
