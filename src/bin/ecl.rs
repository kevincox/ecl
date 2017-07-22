extern crate clap;
extern crate ecl;
extern crate erased_serde;
extern crate serde;
extern crate serde_json;

use erased_serde::Serialize;

fn serialize_val_with(val: ecl::Val, format: &str, compact: bool)
	-> Result<(),erased_serde::Error>
{
	let r = match (format, compact) {
		("json", false) =>
			val.erased_serialize(&mut serde_json::Serializer::pretty(std::io::stdout())),
		("json", true) =>
			val.erased_serialize(&mut serde_json::Serializer::new(std::io::stdout())),
		("lines", _) =>
			return val.erased_serialize(&mut ecl::lines::Serializer::new(std::io::stdout())),
		(other, _) => panic!("Unknown formatter: {:?}", other),
	};
	if r.is_ok() { println!() }
	r
}

fn main() {
	let matches = clap::App::new("ecl")
		.about("Convert ecl configs into data files.")
		.author("Kevin Cox <kevincox@kevincox.ca>")
		.setting(clap::AppSettings::SubcommandRequired)
		.arg(clap::Arg::from_usage("-f --format [format] 'Output format'")
			 .possible_values(&["json", "lines"])
			 .default_value("json")
			 .global(true))
		.arg(clap::Arg::from_usage("-c, --compact 'Format for robots.'").global(true))
		.subcommand(clap::SubCommand::with_name("load")
			.arg_from_usage("<file> 'ecl config to load'")
			.arg_from_usage("-a --args [args] 'Call the config with the given arguments'")
			.arg_from_usage("-s --select [code] 'Run given code with the result as `r`'"))
		.subcommand(clap::SubCommand::with_name("eval")
			.arg_from_usage("<code> 'The code to evaluate'"))
		.get_matches();
	
	match matches.subcommand() {
		("load", Some(load)) => {
			let mut val = ecl::parse_file(&load.value_of("file").unwrap());
			if let Some(code) = load.value_of("args") {
				let args = ecl::parse("<select>", code)
					.expect("Failed to parse --args");
				val = val.call(args)
			}
			if let Some(code) = load.value_of("select") {
				let select = ecl::hacky_parse_func("--select", "r".to_owned(), code);
				val = select.call(val);
			}
			serialize_val_with(val,
				matches.value_of("format").unwrap(),
				matches.is_present("compact")).unwrap();
		},
		("eval", Some(eval)) => {
			let source = eval.value_of("code").unwrap();
			let val = ecl::parse("<command-line>", source)
				.expect("Failed to parse expression.");
			serialize_val_with(val,
				matches.value_of("format").unwrap(),
				matches.is_present("compact")).unwrap();
		},
		(command, args) => {
			panic!("Unknown command {:?} with {:?}", command, args);
		},
	}
}
