extern crate ecl;

mod utils;

#[test]
fn test_compile_lines() {
	for path in utils::scan_dir("lines", "ecl") {
		let ast = ecl::parse_file(&path.to_string_lossy()).unwrap();
		let compiled = ecl::bytecode::compile_to_vec(ast);
		let decompiled = ecl::bytecode::decompile(&compiled).expect("decompile failed");
		
		utils::diff(&decompiled.as_bytes(), &path.with_extension("decompiled"));
	}
}
