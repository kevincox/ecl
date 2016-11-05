extern crate erased_serde;
extern crate gc;
extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::fmt;
use std::marker;

use nil;

pub fn get(key: &str) -> ::Val {
	match key {
		"nil" => ::Val::new(nil::Nil),
		"true" => ::Val::new(true),
		"false" => ::Val::new(false),
		"reverse" => ::Val::new(Func("reverse", |v| v.reverse())),
		other => panic!("Undefined variable {:?}", other),
	}
}

pub struct Func<F: Fn(::Val) -> ::Val + 'static + marker::Reflect>(&'static str, F);
unsafe impl<F: Fn(::Val) -> ::Val + 'static + marker::Reflect> gc::Trace for Func<F> { unsafe_empty_trace!(); }

impl<F: Fn(::Val) -> ::Val + 'static + marker::Reflect> ::SameOps for Func<F> {
	fn eq(&self, that: &Self) -> bool {
		self.0 as *const str == that.0 as *const str
	}
}

impl<F: Fn(::Val) -> ::Val + 'static + marker::Reflect> ::Value for Func<F> {
	fn call(&self, arg: ::Val) -> ::Val {
		self.1(arg)
	}
	
	fn type_str(&self) -> &'static str { "builtin" }
}

impl<F: Fn(::Val) -> ::Val + 'static + marker::Reflect> fmt::Debug for Func<F> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<builtin {:?}>", self.0)
	}
}

