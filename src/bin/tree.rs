use std::env;
use std::fs;
use std::io::Read;

extern crate ecl;
extern crate erased_serde;
extern crate serde;

use erased_serde::Serialize;

fn main() {
	let args: Vec<String> = env::args().collect();
	for path in &args[1..] {
		let mut ecl = fs::File::open(path).unwrap();
		
		let mut buf = String::new();
		ecl.read_to_string(&mut buf).unwrap();
		
		let val = ecl::parse(&buf).unwrap();
		val.erased_serialize(&mut ecl::lines::Serializer::new(std::io::stdout())).unwrap();
		println!("{:?}", val);
	}
}
