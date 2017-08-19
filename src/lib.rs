#![feature(drop_types_in_const)]
#![feature(fnbox)]
#![feature(io)]
#![feature(plugin)]
#![feature(proc_macro)]
#![feature(slice_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(try_trait)]

extern crate erased_serde;
#[macro_use] extern crate gc;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::any;
use std::cell::{RefCell};
use std::fmt;
use std::io::Read;
use std::mem;
use std::rc;

mod builtins;
mod bool;
mod err;
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

pub trait Value:
	any::Any +
	fmt::Debug +
	gc::Trace +
	SameOpsTrait +
	'static
{
	fn type_str(&self) -> &'static str;
	
	fn get(&self) -> Option<Val> { None }
	fn is_err(&self) -> bool { false }
	fn is_empty(&self) -> bool { panic!("Don't know if {:?} is empty", self) }
	fn len(&self) -> usize { panic!("{:?} doesn't have a length", self) }
	fn index_int(&self, _k: usize) -> Val { err::Err::new(format!("Can't index {:?} with an int", self)) }
	fn index_str(&self, _k: &str) -> Val { err::Err::new(format!("Can't index {:?} with string", self)) }
	fn lookup(&self, _key: &str) -> Val { err::Err::new(format!("Can't lookup in {:?}", self)) }
	fn structural_lookup(&self, _depth: usize, _key: &dict::Key) -> Option<Val> { None }
	fn find(&self, _k: &str) -> (usize, dict::Key, Val) { panic!("Can't lookup in {:?}", self) }
	fn serialize(&self, _v: &mut Vec<*const Value>, _s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> { panic!("Can't serialize {:?}", self) }
	fn get_str(&self) -> Option<&str> { None }
	fn get_num(&self) -> Option<f64> { None }
	fn to_slice(&self) -> &[Val] { panic!("Can't turn {:?} into a slice", self) }
	fn to_string(&self) -> Val { err::Err::new(format!("Can't turn {:?} into a string", self)) }
	fn to_bool(&self) -> bool { true }
	fn call(&self, _this: Val, arg: Val) -> Val
		{ err::Err::new(format!("Can't call {:?} with {:?}", self, arg)) }
	fn iter<'a>(&'a self) -> Option<Box<Iterator<Item=Val> + 'a>> { None }
	fn reverse_iter<'a>(&'a self) -> Option<Box<Iterator<Item=Val> + 'a>> { None }
	fn reverse(&self) -> Val { err::Err::new(format!("Can't reverse {:?}", self)) }
}

pub trait SameOps: fmt::Debug {
	fn add(&self, that: &Self) -> Val { err::Err::new(format!("Can't add {:?} and {:?}", self, that)) }
	fn eq(&self, that: &Self) -> Val { err::Err::new(format!("Can't compare {:?} and {:?}", self, that)) }
}

pub trait SameOpsTrait {
	fn as_any(&self) -> &any::Any;
	
	fn add(&self, that: &Value) -> Val;
	fn eq(&self, that: &Value) -> Val;
}

impl<T: SameOps + Value> SameOpsTrait for T {
	fn as_any(&self) -> &any::Any { self }
	
	fn add(&self, that: &Value) -> Val {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::add(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			err::Err::new(format!("Can't add {:?} and {:?}", self, that))
		}
	}
	
	fn eq(&self, that: &Value) -> Val {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::eq(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			bool::get_false()
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
		
		while let Some(ref vn) = Value::get(v.deref()) {
			v = vn.clone();
		}
		// println!("got {:?}", v);
		
		v
	}
	
	fn value(&self) -> Result<&Value,Val> {
		Ok(i_promise_this_will_stay_alive(self.get()?.deref()))
	}
	
	fn deref(&self) -> &Value { &*self.0 }
	
	fn annotate(&self, loc: grammar::Loc, msg: &str) -> Val {
		let v = self.get();
		if v.is_err() {
			err::Err::new_from(v, loc, msg.to_owned())
		} else {
			v
		}
	}
	
	fn downcast_ref<T: 'static>(&self) -> Option<&T> {
		self.deref().as_any().downcast_ref::<T>()
	}
	
	pub fn type_str(&self) -> &'static str {
		self.value().unwrap().type_str()
	}
	
	pub fn get_num(&self) -> Option<f64> {
		self.value().unwrap().get_num()
	}
	
	pub fn is_empty(&self) -> bool {
		self.value().unwrap().is_empty()
	}
	
	pub fn is_err(&self) -> bool {
		self.get().deref().is_err()
	}
	
	pub fn len(&self) -> usize {
		self.value().unwrap().len()
	}
	
	pub fn index(&self, k: Val) -> Val {
		let this = self.value()?;
		let k = k.value()?;
		
		if let Some(s) = k.get_str() {
			this.index_str(s)
		} else if let Some(n) = k.get_num() {
			this.index_int(n as usize)
		} else {
			panic!("Can't index with a {:?}", k)
		}
	}
	
	pub fn index_int(&self, k: usize) -> Val {
		self.value().unwrap().index_int(k)
	}
	
	pub fn index_str(&self, key: &str) -> Val {
		self.value().unwrap().index_str(key)
	}
	
	fn lookup(&self, key: &str) -> Val {
		// println!("Lookup {:?} in {:?}", key, self);
		self.value()?.lookup(key)
	}
	
	fn structural_lookup(&self, depth: usize, key: &dict::Key) -> Option<Val> {
		// println!("Lookup {:?} in {:?}", key, self);
		self.deref().structural_lookup(depth, key)
	}
	
	fn add(&self, that: Val) -> Val {
		self.value()?.add(that.value()?)
	}
	
	pub fn call(&self, arg: Val) -> Val {
		self.value().unwrap().call(self.clone(), arg)
	}
	
	fn get_str(&self) -> Result<&str,Val> {
		let v = self.get()?;
		v.deref().get_str()
			.map(|r| i_promise_this_will_stay_alive(r))
			.ok_or_else(|| err::Err::new(format!("Attempt to treat {:?} as a string", v)))
	}
	
	fn to_slice(&self) -> &[Val] {
		i_promise_this_will_stay_alive(self.value().unwrap().to_slice())
	}
	
	fn to_string(&self) -> Val {
		let v = self.value()?;
		if v.type_str() == "string" { return self.clone() }
		v.to_string()
	}
	
	fn to_bool(&self) -> bool {
		self.value().unwrap().to_bool()
	}
	
	fn iter<'a>(&'a self) -> Option<Box<Iterator<Item=Val> + 'a>> {
		i_promise_this_will_stay_alive(self.get().deref()).iter()
	}
	
	fn foldl(&self, f: Val, accum: Val) -> Val {
		let iterable = self.get();
		let iterable = iterable.deref();
		let iter = match iterable.iter() {
			Some(iter) => iter,
			None => return err::Err::new(format!("Can't iterate over {:?}", iterable)),
		};
		
		iter.fold(accum, |accum, elem| f.call(accum).call(elem))
	}
	
	fn foldr(&self, f: Val, accum: Val) -> Val {
		let iterable = self.get();
		let iterable = iterable.deref();
		let iter = match iterable.reverse_iter() {
			Some(iter) => iter,
			None => return err::Err::new(format!("Can't reverse iterate over {:?}", iterable)),
		};
		
		iter.fold(accum, |accum, elem| f.call(accum).call(elem))
	}
	
	fn map(&self, f: Val) -> Val {
		let iterable = self.get();
		let iterable = iterable.deref();
		let iter = match iterable.iter() {
			Some(iter) => iter,
			None => return err::Err::new(format!("Can't reverse iterate over {:?}", iterable)),
		};
		let vals = iter
			.map(move |v|
				 thunk::Thunk::new(vec![f.clone(), v.clone()], move |r|
					r[0].clone().call(r[1].clone())))
			.collect();
		list::List::of_vals(vals)
	}
	
	
	fn reverse(&self) -> Val {
		self.value()?.reverse()
	}
	
	pub fn rec_ser<'a>(&self, visited: &'a mut Vec<*const Value>) -> SerializeVal<'a> {
		let selfr = self.get();
		let selfp = selfr.deref() as *const Value;
		if visited.contains(&selfp) { panic!("Recursive structure detected."); }
		visited.push(selfp);
		
		SerializeVal { val: selfr, visited: RefCell::new(visited) }
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		self.value().unwrap().eq(that.value().unwrap()).to_bool()
	}
}

impl fmt::Debug for Val {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.deref().fmt(f)
	}
}

impl std::ops::Try for Val {
	type Ok = Self;
	type Error = Self;
	
	fn from_ok(v: Self::Ok) -> Self { v }
	fn from_error(v: Self::Error) -> Self { v }
	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		let val = self.get();
		if val.deref().is_err() { Err(val) } else { Ok(val) }
	}
}

pub struct SerializeVal<'a> {
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
	Add(grammar::Loc, Box<Almost>, Box<Almost>),
	Call(grammar::Loc, Box<Almost>, Box<Almost>),
	Eq(Box<Almost>, Box<Almost>),
	Func(rc::Rc<func::FuncData>),
	Index(grammar::Loc, Box<Almost>, Box<Almost>),
	List(Vec<rc::Rc<Almost>>),
	Nil,
	Num(f64),
	Ref(grammar::Loc, String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl Almost {
	fn complete(&self, plex: Val, pstruct: Val) -> Val {
		match *self {
			Almost::Add(loc, ref l, ref r) => {
				let l = l.complete(plex.clone(), pstruct.clone())
					.annotate(loc, "Left side of add")?;
				let r = r.complete(plex, pstruct)
					.annotate(loc, "Right side of add")?;
				l.add(r)
			}
			Almost::Dict(ref items) => dict::Dict::new(plex, pstruct, &items),
			Almost::Call(loc, ref f, ref a) => {
				let f = f.complete(plex.clone(), pstruct.clone())
					.annotate(loc, "Calling error as function")?;
				// Note, allow calling function with an error.
				f.call(a.complete(plex, pstruct))
			},
			Almost::Eq(ref l, ref r) => {
				let l = l.complete(plex.clone(), pstruct.clone());
				let r = r.complete(plex, pstruct);
				bool::get(l == r)
			},
			Almost::Func(ref fd) => func::Func::new(plex, fd.clone()),
			Almost::Index(loc, ref o, ref k) => {
				let o = o.complete(plex.clone(), pstruct.clone())
					.annotate(loc, "Indexing error value")?;
				let k = k.complete(plex, pstruct)
					.annotate(loc, "Indexing with error as key")?;
				o.index(k).annotate(loc, "Error returned from index")
			},
			Almost::List(ref items) => list::List::new(plex, pstruct, items),
			Almost::Nil => nil::get(),
			Almost::Num(n) => Val::new(n),
			Almost::Ref(loc, ref id) => {
				let (depth, dictkey, val) = plex.value()?.find(id);
				let v = pstruct.structural_lookup(depth, &dictkey).unwrap_or(val);
				v.annotate(loc, "Error value referenced")
			},
			Almost::Str(ref c) => {
				let mut r = String::new();
				for part in c {
					match part {
						&StringPart::Esc(s) => r.push(s),
						&StringPart::Exp(ref e) =>
							r += e.complete(plex.clone(), pstruct.clone())
								.to_string().get_str()?,
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
			Almost::Add(_, ref lhs, ref rhs) => write!(f, "({:?} + {:?})", lhs, rhs),
			Almost::Dict(ref items) => {
				try!(writeln!(f, "{{"));
				for i in &**items {
					try!(writeln!(f, "\t{:?}", i));
				}
				write!(f, "}}")
			},
			Almost::Call(_, ref func, ref a) => write!(f, "({:?}:{:?})", func, a),
			Almost::Eq(ref l, ref r) => write!(f, "({:?} == {:?})", l, r),
			Almost::Func(ref fd) => write!(f, "(->{:?} {:?})", fd.arg, fd.body),
			Almost::Index(_, ref obj, ref key) => write!(f, "{:?}.{:?}", obj, key),
			Almost::List(ref items) => {
				writeln!(f, "[")?;
				for item in items {
					writeln!(f, "\t{:?}", item)?;
				}
				write!(f, "]")
			},
			Almost::Nil => write!(f, "nil"),
			Almost::Num(n) => write!(f, "{}", n),
			Almost::Ref(_, ref id) => write!(f, "Ref({})", format_key(id)),
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

pub fn parse(source: &str, doc: &str) -> Result<Val, grammar::ParseError> {
	assert!(source.find('/').is_none(), "Non-file source can't have a path.");
	let almost = grammar::parse("", doc.chars())?;
	Ok(almost.complete(nil::get(), nil::get()))
}

pub fn parse_file(path: &str) -> Val {
	let file = match std::fs::File::open(path) {
		Ok(file) => file,
		Err(e) => return err::Err::new(format!("Failed to open {:?}: {:?}", path, e)),
	};
	
	let mut err = None;
	let chars = file.chars()
		.filter_map(|r| r.map_err(|e| err = Some(e)).ok())
		.fuse();
	
	let almost = match grammar::parse(path, chars) {
		Ok(almost) => almost,
		Err(e) => return err::Err::new(format!("Failed to parse {:?}: {:?}", path, e)),
	};
	
	almost.complete(nil::get(), nil::get())
}

pub fn hacky_parse_func(source: &str, name: String, doc: &str) -> Val
{
	assert!(source.find('/').is_none(), "Non-file source can't have a path.");
	
	let almost = match grammar::parse(source, doc.chars()) {
		Ok(almost) => almost,
		Err(e) => return err::Err::new(format!("Failed to parse {:?}: {:?}", source, e)),
	};
	
	func::Func::new(nil::get(), rc::Rc::new(func::FuncData{
		arg: func::Arg::One(name),
		body: almost,
	}))
}

pub fn dump_ast(doc: &str) -> Result<(), grammar::ParseError> {
	let almost = try!(grammar::parse("", doc.chars()));
	println!("{:?}", almost);
	Ok(())
}

#[derive(Debug)]
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
		assert!(parse("<str>", "[]").unwrap().is_empty());
		let v = parse("<str>", "[0d29 0b1.1]").unwrap();
		assert_eq!(v.index_int(0), Val::new(29.0));
		assert_eq!(v.index_int(1), Val::new(1.5));
	}
	
	#[test]
	fn ident() {
		assert_eq!(parse("<str>","{b = 4}.b").unwrap(), Val::new(4.0));
	}
	
	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion() {
		parse("<str>" ,"{b = b}").unwrap().index_str("b").get_num();
	}
	
	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion_multi_step() {
		parse("<str>", "{
			a = b
			b = c
			c = d
			d = b
		}").unwrap().index_str("a").get_num();
	}
}
