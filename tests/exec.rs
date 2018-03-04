extern crate ecl;
extern crate serde;
extern crate serde_yaml;
#[macro_use] extern crate serde_derive;

mod utils;

#[derive(Deserialize)]
struct Config {
	cmd: String,
	
	#[serde(default="Default::default")]
	status: i32,
}

#[test]
fn test_exec() {
	let ecl = std::path::PathBuf::from("target/debug/ecl")
		.canonicalize().expect("Could not canonicalize binary.");
	
	for path in utils::scan_dir("exec", "exec") {
		let f = std::fs::File::open(&path).unwrap();
		let config: Config = serde_yaml::from_reader(f).unwrap();
		
		let cmd = format!("exec {:?} {}", ecl, config.cmd);
		let out = std::process::Command::new("bash")
			.arg("-c").arg(&cmd)
			.current_dir("/home/kevincox/p/ecl/tests/exec/")
			.output().expect(&format!("Failed to {:?}", cmd));
		
		utils::diff(&out.stderr, &path.with_extension("stderr"));
		utils::diff(&out.stdout, &path.with_extension("stdout"));
		assert_eq!(out.status.code(), Some(config.status), "Wrong exit status.");
	}
}