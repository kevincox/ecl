use std::env;
use std::fs;
use std::io::Read;

extern crate ecl;
extern crate serde;

use serde::Serialize;

fn main() {
	let args: Vec<String> = env::args().collect();
	for path in &args[1..] {
		println!("Path {}", path);
		let mut ecl = fs::File::open(path).unwrap();
		
		let mut buf = String::new();
		ecl.read_to_string(&mut buf).unwrap();
		
		let val = ecl::parse(&buf).unwrap();
		val.serialize(&mut ecl::lines::Serializer::new(&mut buf)).unwrap();
		
		println!("{}", buf);
	}
}
