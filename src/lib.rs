#![feature(drop_types_in_const)]
#![feature(fnbox)]
#![feature(plugin)]
#![feature(proc_macro)]
#![feature(slice_patterns)]
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
use std::mem;
use std::rc;

mod builtins;
mod bool;
mod dict;
mod func;
pub mod grammar;
pub mod lines;
mod list;
mod nil;
mod num;
mod str;
mod thunk;

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
	fn to_slice(&self) -> &[Val] { panic!("Can't turn {:?} into a slice", self) }
	fn to_string(&self) -> String { panic!("Can't turn {:?} into a string", self) }
	fn to_bool(&self) -> bool { true }
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
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::eq(self, that.as_any().downcast_ref::<Self>().unwrap())
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
		// println!("getting {:?}", self.0.type_str());
		let mut v = self.clone();
		
		let mut iterations = 0; // Delay cycle checking for performance.
		let mut visited = Vec::new(); // Track visited items.
		
		while let Some(ref vn) = Value::get(v.deref()) {
			iterations += 1;
			if iterations > 100 {
				let vn_ptr = vn.deref() as *const Value;
				if visited.contains(&vn_ptr) {
					panic!("Dependency cycle detected.");
				}
				visited.push(vn_ptr);
			}
			
			v = vn.clone();
		}
		// println!("got {:?}", v);
		
		v
	}
	
	fn value(&self) -> &Value { i_promise_this_will_stay_alive(self.get().deref()) }
	
	fn deref(&self) -> &Value { &*self.0 }
	
	fn downcast_ref<T: 'static>(&self) -> &T {
		self.deref().as_any().downcast_ref::<T>().unwrap()
	}
	
	pub fn type_str(&self) -> &'static str {
		self.value().type_str()
	}
	
	fn get_str(&self) -> Option<&str> {
		self.value().get_str().map(|s| i_promise_this_will_stay_alive(s))
	}
	
	fn get_num(&self) -> Option<f64> {
		self.value().get_num()
	}
	
	pub fn is_empty(&self) -> bool {
		self.value().is_empty()
	}
	
	pub fn len(&self) -> usize {
		self.value().len()
	}
	
	pub fn index(&self, k: Val) -> Val {
		let k = k.get();
		
		if let Some(s) = k.get_str() {
			self.index_str(s)
		} else if let Some(n) = k.get_num() {
			self.index_int(n as usize)
		} else {
			panic!("Can't index with a {:?}", k)
		}
	}
	
	fn index_int(&self, k: usize) -> Val {
		self.value().index_int(k)
	}
	
	pub fn index_str(&self, key: &str) -> Val {
		let v = self.get();
		v.deref().index_str(key)
	}
	
	fn lookup(&self, key: &str) -> Val {
		// println!("Lookup {:?} in {:?}", key, self);
		let v = self.get();
		v.deref().lookup(key)
	}
	
	fn add(&self, that: Val) -> Val {
		self.value().add(that.value())
	}
	
	fn call(&self, arg: Val) -> Val {
		self.value().call(arg)
	}
	
	fn to_slice(&self) -> &[Val] {
		i_promise_this_will_stay_alive(self.value().to_slice())
	}
	
	fn to_string(&self) -> String {
		self.value().to_string()
	}
	
	fn to_bool(&self) -> bool {
		self.value().to_bool()
	}
	
	fn reverse(&self) -> Val {
		self.value().reverse()
	}
	
	fn rec_ser<'a>(&self, visited: &'a mut Vec<*const Value>) -> SerializeVal<'a> {
		let selfr = self.get();
		let selfp = selfr.deref() as *const Value;
		if visited.contains(&selfp) { panic!("Recursive structure detected."); }
		visited.push(selfp);
		
		SerializeVal { val: selfr, visited: RefCell::new(visited) }
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		self.value().eq(that.value())
	}
}

impl fmt::Debug for Val {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.deref().fmt(f)
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
		self.val.deref().serialize(&mut*self.visited.borrow_mut(), s).map_err(unerase)
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
	Add(Box<Almost>, Box<Almost>),
	Call(Box<Almost>, Box<Almost>),
	Eq(Box<Almost>, Box<Almost>),
	Func(rc::Rc<func::FuncData>),
	Index(Box<Almost>, Box<Almost>),
	List(Vec<rc::Rc<Almost>>),
	Nil,
	Num(f64),
	Ref(String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl Almost {
	fn complete(&self, p: Val) -> Val {
		match *self {
			Almost::Add(ref l, ref r) => l.complete(p.clone()).add(r.complete(p)),
			Almost::Dict(ref items) => dict::Dict::new(p, &items),
			Almost::Call(ref f, ref a) => f.complete(p.clone()).call(a.complete(p)),
			Almost::Eq(ref l, ref r) => Val::new(l.complete(p.clone()) == r.complete(p)),
			Almost::Func(ref fd) => func::Func::new(p, fd.clone()),
			Almost::Index(ref o, ref k) => o.complete(p.clone()).index(k.complete(p)),
			Almost::List(ref items) => list::List::new(p, items),
			Almost::Nil => nil::get(),
			Almost::Num(n) => Val::new(n),
			Almost::Ref(ref id) => thunk::Thunk::evaluated(p.lookup(id)),
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
			Almost::StrStatic(ref s) => Val::new(s.clone()),
		}
	}
	
	fn is_cheep(&self) -> bool {
		match *self {
			Almost::Nil |
			Almost::Num(_) |
			Almost::StrStatic(_) => true,
			_ => false,
		}
	}
}

impl fmt::Debug for Almost {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Almost::Add(ref lhs, ref rhs) => write!(f, "({:?} + {:?})", lhs, rhs),
			Almost::Dict(ref items) => {
				try!(writeln!(f, "{{"));
				for i in &**items {
					try!(writeln!(f, "\t{:?}", i));
				}
				write!(f, "}}")
			},
			Almost::Call(ref func, ref a) => write!(f, "({:?}:{:?})", func, a),
			Almost::Eq(ref l, ref r) => write!(f, "({:?} == {:?})", l, r),
			Almost::Func(ref fd) => write!(f, "(->{:?} {:?})", fd.arg, fd.body),
			Almost::Index(ref obj, ref key) => write!(f, "{:?}.{:?}", obj, key),
			Almost::List(ref items) => {
				writeln!(f, "[")?;
				for item in items {
					writeln!(f, "\t{:?}", item)?;
				}
				write!(f, "]")
			},
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

pub fn parse(doc: &str) -> Result<Val, grammar::ParseError> {
	let almost = grammar::parse(doc.chars())?;
	Ok(thunk::Thunk::new(vec![], move |_| almost.complete(nil::get())))
}

pub fn dump_ast(doc: &str) -> Result<(), grammar::ParseError> {
	let almost = try!(grammar::parse(doc.chars()));
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
	fn val_is_gc_sized() {
		// Testing an implementation detail. A value should be one word.
		// If more falls out of https://github.com/rust-lang/rfcs/issues/1230
		// we can probably pack more types into the enum safely. Otherwise we
		// can consider having a pointer and doing the checking ourselves unsafely.
		assert_eq!(std::mem::size_of::<gc::Gc<Value>>(), std::mem::size_of::<Val>());
	}
	
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
