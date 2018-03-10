extern crate ecl;

mod utils;

fn main() {
	utils::test_dir("lines", "ecl", |path| {
		let ast = ecl::parse_file(&path.to_string_lossy()).unwrap();
		let compiled = ecl::bytecode::compile_to_vec(ast).unwrap();
		let decompiled = ecl::bytecode::DisassembleOptions::diffable()
			.disassemble(&compiled)
			.unwrap_or_else(|d| panic!("decompile failed:\n{:?}", d));

		utils::diff(&decompiled.as_bytes(), &path.with_extension("decompiled"));
	})
}
