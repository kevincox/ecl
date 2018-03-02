extern crate clap;
extern crate ecl;
extern crate erased_serde;
extern crate serde;
extern crate serde_json;
extern crate serde_yaml;

use erased_serde::Serialize;

fn serialize_val_with(val: ecl::Val, format: &str, compact: bool)
{
	match (format, compact) {
		("json", false) =>
			val.erased_serialize(&mut serde_json::Serializer::pretty(std::io::stdout()))
				.unwrap(),
		("json", true) =>
			val.erased_serialize(&mut serde_json::Serializer::new(std::io::stdout()))
				.unwrap(),
		("lines", _) =>
			val.erased_serialize(&mut ecl::lines::Serializer::new(std::io::stdout()))
				.unwrap(),
		("yaml", _) => {
			let mut vec = Vec::new();
			let seriliazable = val.rec_ser(&mut vec);
			serde_yaml::to_writer(&mut std::io::stdout(), &seriliazable)
				.unwrap()
		}
		(other, _) => panic!("Unknown formatter: {:?}", other),
	}
	println!();
}

fn main() {
	let formatarg = clap::Arg::from_usage("-f --format [format] 'Output format'")
		.possible_values(&["json", "lines", "yaml"])
		.default_value("json")
		.global(true);
	let compactarg = clap::Arg::from_usage("-c, --compact 'Format for robots.'");
	let matches = clap::App::new("ecl")
		.about("Convert ecl configs into data files.")
		.author("Kevin Cox <kevincox@kevincox.ca>")
		.setting(clap::AppSettings::SubcommandRequired)
		.subcommand(clap::SubCommand::with_name("load")
			.arg_from_usage("<file> 'ecl config to load'")
			.arg_from_usage("-a --args [args] 'Call the config with the given arguments'")
			.arg_from_usage("-s --select [code] 'Run given code with the result as `r`'")
			.arg(formatarg.clone())
			.arg(compactarg.clone()))
		.subcommand(clap::SubCommand::with_name("eval")
			.arg_from_usage("<code> 'The code to evaluate'")
			.arg(formatarg)
			.arg(compactarg))
		.get_matches();
	
	match matches.subcommand() {
		("load", Some(load)) => {
			let mut val = ecl::eval_file(&load.value_of("file").unwrap());
			if let Some(code) = load.value_of("args") {
				let args = ecl::eval("<args>", code);
				val = val.call(args)
			}
			if let Some(code) = load.value_of("select") {
				let select = ecl::hacky_parse_func("--select", "r".to_owned(), code);
				val = select.call(val);
			}
			
			let val = val.get();
			if let Err(e) = val.eval() {
				eprintln!("{:?}\nError occured.", e);
				std::process::exit(1)
			}
			
			serialize_val_with(val,
				load.value_of("format").unwrap(),
				load.is_present("compact"));
		},
		("eval", Some(eval)) => {
			let source = eval.value_of("code").unwrap();
			let val = ecl::eval("<command-line>", source);
			serialize_val_with(val,
				eval.value_of("format").unwrap(),
				eval.is_present("compact"));
		},
		(command, args) => {
			panic!("Unknown command {:?} with {:?}", command, args);
		},
	}
}
