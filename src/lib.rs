#![feature(plugin)]
#![feature(proc_macro)]
// #![plugin(afl_plugin)]
#![feature(stmt_expr_attributes)]

extern crate erased_serde;
#[macro_use] extern crate gc;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::any;
use std::cell::{RefCell};
use std::fmt;
use std::iter::Iterator;
use std::mem;
use std::rc;

mod builtins;
mod bool;
mod dict;
mod func;
pub mod lines;
mod list;
mod nil;
mod num;
mod str;
mod thunk;

mod grammar { include!(concat!(env!("OUT_DIR"), "/grammar.rs")); }
pub use grammar::ParseError;

fn i_promise_this_will_stay_alive<T: ?Sized>(v: &T) -> &'static T {
	unsafe { mem::transmute(v) }
}

pub trait Value: gc::Trace + fmt::Debug + any::Any + SameOpsTrait + 'static {
	fn get(&self) -> Option<Val> { None }
	fn type_str(&self) -> &'static str { panic!("Unknown type str for {:?}", self) }
	fn is_empty(&self) -> bool { panic!("Don't know if {:?} is empty", self) }
	fn len(&self) -> usize { panic!("{:?} doesn't have a length", self) }
	fn index_int(&self, _k: usize) -> Val { panic!("Can't index {:?} with an int", self) }
	fn index_str(&self, _k: &str) -> Val { panic!("Can't index {:?} with string", self) }
	fn lookup(&self, _key: &str) -> Val { panic!("Can't lookup in {:?}", self) }
	fn serialize(&self, _v: &mut Vec<*const Value>, _s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> { panic!("Can't serialize {:?}", self) }
	fn get_str(&self) -> Option<&str> { None }
	fn get_num(&self) -> Option<f64> { None }
	fn to_string(&self) -> String { panic!("Can't turn {:?} into a string", self) }
	fn call(&self, _arg: Val) -> Val { panic!("Can't call {:?}", self) }
	fn reverse(&self) -> Val { panic!("Can't reverse {:?}", self) }
}

pub trait SameOps: fmt::Debug {
	fn add(&self, that: &Self) -> Val { panic!("Can't add {:?} and {:?}", self, that) }
	fn eq(&self, that: &Self) -> bool { panic!("Can't compare {:?} and {:?}", self, that) }
}

pub trait SameOpsTrait {
	fn as_any(&self) -> &any::Any;
	
	fn add(&self, that: &Value) -> Val;
	fn eq(&self, that: &Value) -> bool;
}

impl<T: SameOps + Value> SameOpsTrait for T {
	fn as_any(&self) -> &any::Any { self }
	
	fn add(&self, that: &Value) -> Val {
		if that.type_str() as *const str == that.type_str() as *const str {
			SameOps::add(self, unsafe { *mem::transmute::<&&Value, &&T>(&that) })
		} else {
			panic!("Can't add {:?} and {:?}", self, that)
		}
	}
	
	fn eq(&self, that: &Value) -> bool {
		if that.type_str() as *const str == that.type_str() as *const str {
			SameOps::eq(self, unsafe { *mem::transmute::<&&Value, &&Self>(&that) })
		} else {
			false
		}
	}
}

#[derive(Clone,Trace)]
pub struct Val(gc::Gc<Value>);

unsafe impl Sync for Val { }

impl Val {
	fn new<T: Value + Sized>(v: T) -> Val {
		// println!("Allocating {:?}", v);
		Val(gc::Gc::new(v))
	}
	
	fn get(&self) -> Val {
		println!("getting {:?}", self.0.type_str());
		let mut v = self.clone();
		
		let mut iterations = 0; // Delay cycle checking for performance.
		let mut visited = Vec::new(); // Track visited items.
		
		while let Some(ref vn) = Value::get(&*v.0) {
			iterations += 1;
			if iterations > 100 {
				let vn_ptr = &*vn.0 as *const Value;
				if visited.contains(&vn_ptr) {
					panic!("Dependency cycle detected.");
				}
				visited.push(vn_ptr);
			}
			
			v = vn.clone();
		}
		println!("got {:?}", v);
		v
	}
	
	fn downcast_ref<T: 'static>(&self) -> &T {
		self.0.as_any().downcast_ref::<T>().unwrap()
	}
	
	pub fn type_str(&self) -> &'static str {
		self.get().0.type_str()
	}
	
	fn get_str(&self) -> Option<&str> {
		self.get().0.get_str().map(|s| i_promise_this_will_stay_alive(s))
	}
	
	fn get_num(&self) -> Option<f64> {
		self.get().0.get_num()
	}
	
	pub fn is_empty(&self) -> bool {
		self.get().0.is_empty()
	}
	
	pub fn len(&self) -> usize {
		self.get().0.len()
	}
	
	pub fn index(&self, k: Val) -> Val {
		if let Some(s) = k.get_str() {
			self.index_str(s)
		} else if let Some(n) = k.get_num() {
			self.index_int(n as usize)
		} else {
			panic!("Can't index {:?} with {:?}", self, k)
		}
	}
	
	fn index_int(&self, k: usize) -> Val {
		self.get().0.index_int(k)
	}
	
	pub fn index_str(&self, key: &str) -> Val {
		let v = self.get();
		v.0.index_str(key)
	}
	
	fn lookup(&self, key: &str) -> Val {
		println!("Lookup {:?} in {:?}", key, self);
		let v = self.get();
		v.0.lookup(key)
	}
	
	fn add(&self, that: Val) -> Val {
		self.get().0.add(&*that.get().0)
	}
	
	fn call(&self, arg: Val) -> Val {
		self.get().0.call(arg)
	}
	
	fn to_string(&self) -> String {
		self.get().0.to_string()
	}
	
	fn reverse(&self) -> Val {
		self.get().0.reverse()
	}
	
	fn rec_ser<'a>(&self, visited: &'a mut Vec<*const Value>) -> SerializeVal<'a> {
		let selfr = self.get();
		let selfp = &*selfr.0 as *const Value;
		if visited.contains(&selfp) { panic!("Recursive structure detected."); }
		visited.push(selfp);
		
		SerializeVal { val: selfr, visited: RefCell::new(visited) }
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		self.get().0.eq(&*that.get().0)
	}
}

impl fmt::Debug for Val {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.0.fmt(f)
	}
}

struct SerializeVal<'a> {
	val: Val,
	visited: RefCell<&'a mut Vec<*const Value>>,
}

impl<'a> Drop for SerializeVal<'a> {
	fn drop(&mut self) {
		self.visited.get_mut().pop();
	}
}

impl<'a> serde::Serialize for SerializeVal<'a> {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(), S::Error> {
		self.val.0.serialize(&mut*self.visited.borrow_mut(), s).map_err(unerase)
	}
}

fn unerase<E: serde::ser::Error>(e: erased_serde::Error) -> E {
	use std::error::Error;
	E::custom(e.description())
}

impl serde::Serialize for Val {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(), S::Error> {
		let mut v: Vec<*const Value> = vec![];
		self.rec_ser(unsafe{ mem::transmute(&mut v) }).serialize(s)
	}
}

pub enum Almost {
	Dict(Vec<dict::AlmostDictElement>),
	Expr(Box<Almost>, Vec<Suffix>),
	Lazy(Box<Fn(Val) -> Val>),
	Nil,
	Num(f64),
	Ref(String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl Almost {
	fn val<F: Fn(Val) -> Val + 'static>(f: F) -> Almost {
		Almost::Lazy(Box::new(f))
	}
	
	fn dict(items: Vec<dict::AlmostDictElement>) -> Almost {
		Almost::Dict(items)
	}
	
	fn function(arg: func::Arg, body: Almost) -> Almost {
		let body = rc::Rc::new(body);
		Almost::val(move |p| {
			func::Func::new(p.clone(), arg.clone(), body.clone())
		})
	}
	
	fn list(items: Vec<Almost>) -> Almost {
		Almost::val(move |p| {
			list::List::new(p, &items)
		})
	}
	
	fn str(c: Vec<StringPart>) -> Almost {
		Almost::Str(c)
	}
	
	fn str_static(s: String) -> Almost {
		Almost::StrStatic(s)
	}
	
	fn expr(subject: Almost, suffixes: Vec<Suffix>) -> Almost {
		Almost::Expr(Box::new(subject), suffixes)
	}
	
	fn complete(&self, p: Val) -> Val {
		match *self {
			Almost::Dict(ref items) => {
				dict::Dict::new(p, &items)
			},
			Almost::Expr(ref subject, ref suffixes) => {
				let subject = subject.complete(p.clone());
				suffixes.iter().fold(subject, |a, e| {
					e.call(p.clone(), a)
				})
			},
			Almost::Lazy(ref l) => l(p),
			Almost::Nil => Val::new(nil::Nil),
			Almost::Num(n) => Val::new(n),
			Almost::Ref(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				thunk::Thunk::new(vec![p], move |r| r[0].lookup(id))
			},
			Almost::Str(ref c) => {
				let mut r = String::new();
				for part in c {
					match part {
						&StringPart::Esc(s) => r.push(s),
						&StringPart::Exp(ref e) => r += &e.complete(p.clone()).to_string(),
						&StringPart::Lit(ref s) => r += &s,
					}
				}
				Val::new(r)
			},
			Almost::StrStatic(ref s) => {
				Val::new(s.clone())
			},
		}
	}
}

impl fmt::Debug for Almost {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Almost::Dict(ref items) => {
				try!(writeln!(f, "Dict({{"));
				for i in &**items {
					try!(writeln!(f, "\t{:?}", i));
				}
				write!(f, "}})")
			},
			Almost::Expr(ref subject, ref suffixes) => {
				try!(write!(f, "Expr({:?}", subject));
				for s in suffixes {
					try!(write!(f, "{:?}", s));
				}
				write!(f, ")")
			},
			Almost::Lazy(_) => write!(f, "<opaque>"),
			Almost::Nil => write!(f, "nil"),
			Almost::Num(n) => write!(f, "{}", n),
			Almost::Ref(ref id) => write!(f, "Ref({})", format_key(id)),
			Almost::Str(ref parts) => {
				try!(write!(f, "\""));
				for part in parts {
					match *part {
						StringPart::Esc(s)     => try!(write!(f, "{}", s)),
						StringPart::Exp(ref s) => try!(write!(f, "${{{:?}}}", s)),
						StringPart::Lit(ref s) => try!(write!(f, "{}", escape_string_contents(&s))),
					}
				}
				write!(f, "\"")
			},
			Almost::StrStatic(ref s) => {
				write!(f, "{}", escape_string(&s))
			},
		}
	}
}

pub enum Suffix {
	Add(Almost),
	Call(Almost),
	Eq(Almost),
	IndexExpr(Almost),
	IndexIdent(String),
	Neq(Almost),
}

impl Suffix {
	fn call(&self, parent: Val, subject: Val) -> Val {
		match *self {
			Suffix::Add(ref a) => {
				let val = a.complete(parent);
				subject.add(val)
			},
			Suffix::Call(ref a) => {
				let val = a.complete(parent);
				subject.call(val)
			},
			Suffix::Eq(ref a) => {
				let val = a.complete(parent);
				Val::new(subject == val)
			},
			Suffix::IndexExpr(ref key) => {
				let k = key.complete(parent);
				thunk::Thunk::new(vec![subject, k], move |r| r[0].index(r[1].clone()))
			},
			Suffix::IndexIdent(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				thunk::Thunk::new(vec![subject], move |r| r[0].index_str(id))
			}
			Suffix::Neq(ref a) => {
				let val = a.complete(parent);
				Val::new(subject != val)
			},
		}
	}
}

impl fmt::Debug for Suffix {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Suffix::Add(ref a) => write!(f, " + {:?}", a),
			Suffix::Call(ref a) => write!(f, ": {:?}", a),
			Suffix::Eq(ref a) => write!(f, "== {:?}", a),
			Suffix::IndexExpr(ref a) => write!(f, ".{:?}", a),
			Suffix::IndexIdent(ref s) => write!(f, ".{}", format_key(s)),
			Suffix::Neq(ref a) => write!(f, "!= {:?}", a),
		}
	}
}

pub fn parse(doc: &str) -> Result<Val, grammar::ParseError> {
	let almost = try!(grammar::document(doc));
	Ok(thunk::Thunk::new(vec![], move |_| almost.complete(Val::new(nil::Nil))))
}

pub fn dump_ast(doc: &str) -> Result<(), grammar::ParseError> {
	let almost = try!(grammar::document(doc));
	println!("{:?}", almost);
	Ok(())
}

pub enum StringPart { Esc(char), Lit(String), Exp(Almost) }

fn do_escape_string_contents(s: &str, r: &mut String) {
	for c in s.chars() {
		match c {
			'"'  => r.push_str("\\\""),
			'$'  => r.push_str("\\$"),
			'\0' => r.push_str("\\0"),
			'\n' => r.push_str("\\n"),
			'\t' => r.push_str("\\t"),
			_ => r.push(c),
		}
	}
}

fn escape_string_contents(s: &str) -> String {
	let mut r = String::with_capacity(s.len());
	do_escape_string_contents(s, &mut r);
	r
}


pub fn escape_string(s: &str) -> String {
	let mut r = String::with_capacity(s.len() + 2);
	r.push('"');
	do_escape_string_contents(s, &mut r);
	r.push('"');
	r
}

pub fn format_key(s: &str) -> String {
	lazy_static! {
		static ref RE: regex::Regex = regex::Regex::new("^[A-Za-z0-9-_]+$").unwrap();
	}
	if RE.is_match(s) {
		s.to_owned()
	} else {
		escape_string(s)
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	
	#[test]
	fn list() {
		assert!(parse("[]").unwrap().is_empty());
		let v = parse("[0d29 0b1.1]").unwrap();
		assert_eq!(v.index_int(0), Val::new(29.0));
		assert_eq!(v.index_int(1), Val::new(1.5));
	}
	
	#[test]
	fn ident() {
		assert_eq!(parse("{b = 4}.b").unwrap(), Val::new(4.0));
	}
	
	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion() {
		parse("{b = b}").unwrap().index_str("b").get_num();
	}

	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion_multi_step() {
		parse("{
			a = b
			b = c
			c = d
			d = b
		}").unwrap().index_str("a").get_num();
	}
}
