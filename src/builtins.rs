extern crate erased_serde;
extern crate gc;
extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::fmt;

use nil;

pub fn get(key: &str) -> ::Val {
	match key {
		"nil" => ::Val::new(nil::Nil),
		"true" => ::Val::new(true),
		"false" => ::Val::new(false),
		"cond" => ::Val::new(Func("if", |v| cond(v.to_slice()))),
		"reverse" => ::Val::new(Func("reverse", |v| v.reverse())),
		other => panic!("Undefined variable {:?}", other),
	}
}

pub struct Func<F: Fn(::Val) -> ::Val + 'static>(&'static str, F);
unsafe impl<F: Fn(::Val) -> ::Val + 'static> gc::Trace for Func<F> { unsafe_empty_trace!(); }

impl<F: Fn(::Val) -> ::Val + 'static> ::SameOps for Func<F> {
	fn eq(&self, that: &Self) -> bool {
		self.0 as *const str == that.0 as *const str
	}
}

impl<F: Fn(::Val) -> ::Val + 'static> ::Value for Func<F> {
	fn call(&self, arg: ::Val) -> ::Val {
		self.1(arg)
	}
	
	fn type_str(&self) -> &'static str { "builtin" }
}

impl<F: Fn(::Val) -> ::Val + 'static> fmt::Debug for Func<F> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<builtin {:?}>", self.0)
	}
}

fn cond(args: &[::Val]) -> ::Val {
	match *args {
		[] => ::Val::new(nil::Nil),
		[ref val] => val.clone(),
		[ref pred, ref val, ref rest..] => {
			if *pred == ::Val::new(nil::Nil) || *pred == ::Val::new(false) {
				cond(rest)
			} else {
				val.clone()
			}
		},
	}
}
