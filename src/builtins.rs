extern crate erased_serde;
extern crate gc;
extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::fmt;
use std::cell::Cell;

use nil;

#[derive(PartialEq,Trace)]
struct Empty;

#[derive(PartialEq,Trace)]
struct Two(::Val, ::Val);

pub fn get(key: &str) -> ::Val {
	match key {
		"cond" => new("if", |v| cond(v.to_slice())),
		"error" => new("error", |msg|
			::err::Err::new(format!("Error: {:?}", msg.get()))),
		"index" => new("index", |l| Builtin::new("index curried", l, |l, i| l.index(i))),
		"false" => ::bool::get_false(),
		"foldl" => new("foldl",
			|f| Builtin::new("foldl:func", f,
				|f, accum| Builtin::new("foldl:func:accum", Two(f.clone(), accum.clone()),
					|&Two(ref f, ref accum), o| o.foldl(f.clone(), accum.clone())))),
		"foldr" => new("foldr",
			|f| Builtin::new("foldr:func", f,
				|f, accum| Builtin::new("foldr:func:accum", Two(f.clone(), accum.clone()),
					|&Two(ref f, ref accum), o| o.foldr(f.clone(), accum.clone())))),
		"load" => new("load", |path| {
			if path.is_err() { return path }
			match path.get_str() {
				Ok(s) => ::parse_file(s),
				Err(e) => ::err::Err::new_from(e,
					::grammar::Loc{line:0, col: 0},
					format!("load expects string argument, got {:?}", path)),
			}
		}),
		"map" => new("index", |f| Builtin::new("map:func", f, |f, o| o.map(f.clone()))),
		"nil" => nil::get(),
		"reverse" => new("reverse", |v| v.reverse()),
		"panic" => new("panic", |msg|
			panic!("Script called panic: {:?}", msg.get())),
		"true" => ::bool::get_true(),
		"type" => new("type", |v| ::Val::new(v.type_str().to_owned())),
		"_testing_assert_cache_eval" => {
			let unevaluated = Cell::new(true);
			let func = move |r| {
				assert!(unevaluated.get(), "Called twice");
				unevaluated.set(false);
				r
			};
			new("_testing_assert_eval_once", func)
		},
		other => ::err::Err::new(format!("Undefined variable {:?}", other)),
	}
}

#[derive(Trace)]
pub struct Builtin<D: gc::Trace, F>{
	name: &'static str,
	data: D,
	#[unsafe_ignore_trace]
	func: F,
}

fn new<F: Fn(::Val) -> ::Val + 'static>(name: &'static str, func: F) -> ::Val {
	Builtin::new(name, Empty, move |_, a| func(a))
}

impl<D: PartialEq + gc::Trace + 'static, F: Fn(&D, ::Val) -> ::Val + 'static> Builtin<D, F> {
	fn new(name: &'static str, d: D, func: F) -> ::Val {
		::Val::new(Builtin{name: name, data: d, func: func})
	}
}

impl<
	D: PartialEq + gc::Trace + 'static,
	F: Fn(&D, ::Val) -> ::Val + 'static>
::SameOps for Builtin<D, F> {
	fn eq(&self, that: &Self) -> ::Val {
		::bool::get(self.name as *const str == that.name as *const str && self.data == that.data)
	}
}

impl<D: PartialEq + gc::Trace + 'static, F: Fn(&D, ::Val) -> ::Val + 'static> ::Value for Builtin<D, F> {
	fn call(&self, arg: ::Val) -> ::Val {
		(self.func)(&self.data, arg)
	}
	
	fn type_str(&self) -> &'static str { "builtin" }
}

impl<D: gc::Trace, F: Fn(&D, ::Val) -> ::Val + 'static> fmt::Debug for Builtin<D, F> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<builtin {:?}>", self.name)
	}
}

fn cond(args: &[::Val]) -> ::Val {
	match *args {
		[] => nil::get(),
		[ref val] => val.clone(),
		[ref pred, ref val, ref rest..] => {
			if pred.to_bool() {
				val.clone()
			} else {
				cond(rest)
			}
		},
	}
}

#[cfg(test)]
mod tests {
	use ::Value;
	use super::*;
	
	#[test]
	#[should_panic(expected="Baby\\'s first error")]
	fn panic() {
		let v = ::parse("<str>", r###"
			{
				msg = "Baby's first" + " error"
				boom = panic:msg
			}.boom
		"###).unwrap().get();
		println!("Returned value: {:?}", v);
	}
	
	#[test]
	fn assert_once_once() {
		let v = nil::Nil.lookup("_testing_assert_cache_eval");
		assert_eq!(v.call(::Val::new(5.1)), ::Val::new(5.1));
	}
	
	#[test]
	#[should_panic(expected="Called twice")]
	fn assert_once_twice() {
		let v = nil::Nil.lookup("_testing_assert_cache_eval");
		assert_eq!(v.call(::Val::new(5.1)), ::Val::new(5.1));
		assert_eq!(v.call(::Val::new(5.1)), ::Val::new(5.1));
	}
}
