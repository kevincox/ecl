use std::env;
use std::fs;
use std::io::Read;

extern crate ecl;
extern crate erased_serde;
extern crate serde;

use erased_serde::Serialize;

fn main() {
	for path in env::args() {
		let val = ecl::parse_file(&path);
		val.erased_serialize(&mut ecl::lines::Serializer::new(std::io::stdout())).unwrap();
	}
}
