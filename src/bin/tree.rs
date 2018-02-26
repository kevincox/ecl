use std::env;

extern crate ecl;
extern crate erased_serde;
extern crate serde;

use erased_serde::Serialize;

fn main() {
	for path in env::args() {
		let val = ecl::eval_file(&path);
		val.erased_serialize(&mut ecl::lines::Serializer::new(std::io::stdout())).unwrap();
		println!("{:?}", val);
	}
}
