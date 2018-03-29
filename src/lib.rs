#![feature(i128_type)]
#![feature(io)]
#![feature(plugin)]
#![feature(proc_macro)]
#![feature(slice_patterns)]
#![feature(try_from)]
#![feature(try_trait)]

extern crate byteorder;
extern crate erased_serde;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate serde;

use std::io::Read;
use std::rc::Rc;

mod builtins;
mod bool;
pub mod bytecode;
mod err;
mod dict;
mod func;
pub mod grammar;
pub mod lines;
mod list;
mod nil;
mod num;
mod mem;
mod str;
mod thunk;

fn i_promise_this_will_stay_alive<T: ?Sized>(v: &T) -> &'static T {
	unsafe { std::mem::transmute(v) }
}

pub trait Value:
	std::any::Any +
	std::fmt::Debug +
	std::any::Any +
	std::fmt::Debug +
	SameOpsTrait +
	'static
{
	fn type_str(&self) -> &'static str;

	fn eval(&self) -> Result<(),Val> { Ok(()) }
	fn is_err(&self) -> bool { false }
	fn is_empty(&self) -> bool { panic!("Don't know if {:?} is empty", self) }
	fn len(&self) -> usize { panic!("{:?} doesn't have a length", self) }
	fn index_int(&self, _k: usize) -> Val { err::Err::new(format!("Can't index {:?} with an int", self)) }
	fn index_str(&self, _k: &str) -> Val { err::Err::new(format!("Can't index {:?} with string", self)) }
	fn structural_lookup(&self, _depth: usize, key: &dict::Key) -> Val {
		::err::Err::new(format!("Can't lookup {} in {:?}", key, self))
	}
	fn serialize(&self, _v: &mut Vec<*const Value>, _s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> { panic!("Can't serialize {:?}", self) }
	fn get_str(&self) -> Option<&str> { None }
	fn get_num(&self) -> Option<f64> { None }
	fn to_string(&self) -> Val { err::Err::new(format!("Can't turn {:?} into a string", self)) }
	fn to_bool(&self) -> bool { true }
	fn neg(&self) -> Val { err::Err::new(format!("Can't negate {:?}", self)) }
	fn call(&self, arg: Val) -> Val
		{ err::Err::new(format!("Can't call {:?} with {:?}", self, arg)) }
	fn iter<'a>(&'a self) -> Option<(mem::PoolHandle, Box<Iterator<Item=Val> + 'a>)> { None }
	fn reverse_iter<'a>(&'a self) -> Option<(mem::PoolHandle, Box<Iterator<Item=Val> + 'a>)> { None }
	fn reverse(&self) -> Val { err::Err::new(format!("Can't reverse {:?}", self)) }
}

pub trait SameOps: std::fmt::Debug {
	fn add(&self, that: &Self) -> Val { err::Err::new(format!("Can't add {:?} and {:?}", self, that)) }
	fn subtract(&self, that: &Self) -> Val { err::Err::new(format!("Can't subtract {:?} and {:?}", self, that)) }
	fn eq(&self, that: &Self) -> Val { err::Err::new(format!("Can't compare {:?} and {:?}", self, that)) }

	fn cmp(&self, that: &Self) -> Result<std::cmp::Ordering,Val> {
		Err(err::Err::new(format!("Can't compare {:?} and {:?}", self, that)))
	}
}

pub trait SameOpsTrait {
	fn as_any(&self) -> &std::any::Any;

	fn add(&self, that: &Value) -> Val;
	fn subtract(&self, that: &Value) -> Val;
	fn eq(&self, that: &Value) -> Val;
	fn cmp(&self, that: &Value) -> Result<std::cmp::Ordering,Val>;
}

impl<T: SameOps + Value> SameOpsTrait for T {
	fn as_any(&self) -> &std::any::Any { self }

	fn add(&self, that: &Value) -> Val {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::add(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			err::Err::new(format!("Can't add {:?} and {:?}", self, that))
		}
	}

	fn subtract(&self, that: &Value) -> Val {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::subtract(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			err::Err::new(format!("Can't subtract {:?} and {:?}", self, that))
		}
	}

	fn eq(&self, that: &Value) -> Val {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::eq(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			bool::get_false()
		}
	}

	fn cmp(&self, that: &Value) -> Result<std::cmp::Ordering,Val> {
		if self.type_str() as *const str == that.type_str() as *const str {
			SameOps::cmp(self, that.as_any().downcast_ref::<Self>().unwrap())
		} else {
			Err(err::Err::new(format!("Can't compare values of different types {:?} and {:?}", self, that)))
		}
	}
}

#[derive(Clone)]
pub struct Val {
	pool: mem::PoolHandle,
	value: std::rc::Weak<Value>,
}

unsafe impl Sync for Val { }

impl Val {
	fn new<T: Value + Sized>(pool: mem::PoolHandle, value: T) -> Val {
		let rc = Rc::new(value);
		let value = Rc::downgrade(&rc);
		pool.push(rc);
		Val{pool, value}
	}

	fn new_atomic<T: Value + Sized>(value: T) -> Val {
		Self::new(mem::PoolHandle::new(), value)
	}

	fn value(&self) -> Result<Rc<Value>,Val> {
		let this = self.deref();
		if this.is_err() { Err(self.clone()) } else { Ok(this) }
	}

	fn deref(&self) -> Rc<Value> { self.value.upgrade().expect("Val upgrade") }

	fn merge(&self, val: Val) -> Val {
		self.pool.merge(val.pool);
		Val{pool: self.pool.clone(), value: val.value}
	}

	fn annotate(&self, msg: &str) -> Val {
		self.annotate_at(::grammar::Loc{line: 0, col: 0}, msg)
	}

	fn annotate_at(&self, loc: grammar::Loc, msg: &str) -> Val {
		if self.is_err() {
			err::Err::new_from_at(self.clone(), loc, msg.to_owned())
		} else {
			self.clone()
		}
	}

	fn annotate_with<F: FnOnce() -> String>(&self, f: F) -> Val {
		self.annotate_at_with(::grammar::Loc{line: 0, col: 0}, f)
	}

	fn annotate_at_with<F: FnOnce() -> String>(&self, loc: grammar::Loc, f: F) -> Val {
		if self.is_err() {
			err::Err::new_from_at(self.clone(), loc, f())
		} else {
			self.clone()
		}
	}

	fn downcast_ref<T: 'static>(&self) -> Option<&T> {
		self.deref().as_any().downcast_ref::<T>().map(i_promise_this_will_stay_alive)
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
		self.deref().is_err()
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

	fn add(&self, that: Val) -> Val {
		self.value()?.add(&*that.value()?)
	}

	fn subtract(&self, that: Val) -> Val {
		self.value()?.subtract(&*that.value()?)
	}

	fn neg(&self) -> Val {
		self.value()?.neg()
	}

	fn cmp(&self, that: ::Val) -> Result<std::cmp::Ordering,Val> {
		self.value()?.cmp(&*that.value()?)
	}

	pub fn call(&self, arg: Val) -> Val {
		let v = self.value()?.call(arg);
		self.pool.merge(v.pool.clone());
		v
	}

	fn get_str(&self) -> Result<&str,Val> {
		self.deref().get_str()
			.map(|r| i_promise_this_will_stay_alive(r))
			.ok_or_else(|| err::Err::new(format!("Attempt to treat {:?} as a string", self)))
	}

	fn to_string(&self) -> Val {
		let v = self.value()?;
		if v.type_str() == "string" { return self.clone() }
		v.to_string()
	}

	fn to_bool(&self) -> bool {
		self.value().unwrap().to_bool()
	}

	pub fn iter<'a>(&'a self) -> Option<(mem::PoolHandle, Box<Iterator<Item=Val> + 'a>)> {
		i_promise_this_will_stay_alive(&*self.deref()).iter()
	}

	fn foldl(&self, f: Val, accum: Val) -> Val {
		let this = self.deref();
		let (_pool, iter) = match this.iter() {
			Some(t) => t,
			None => return err::Err::new(format!("Can't iterate over {:?}", self)),
		};

		iter.fold(accum, |accum, elem| f.call(accum).call(elem))
	}

	fn foldr(&self, f: Val, accum: Val) -> Val {
		let this = self.deref();
		let (_pool, iter) = match this.reverse_iter() {
			Some(t) => t,
			None => return err::Err::new(format!("Can't reverse iterate over {:?}", self)),
		};

		iter.fold(accum, |accum, elem| f.call(accum).call(elem))
	}

	fn map(&self, f: Val) -> Val {
		let this = self.deref();
		let (pool, iter) = match this.iter() {
			Some(t) => t,
			None => return err::Err::new(format!("Can't iterate over {:?}", self)),
		};
		pool.merge(f.pool.clone());
		fn do_map((p, f, v): (
			mem::WeakPoolHandle,
			std::rc::Weak<Value>,
			std::rc::Weak<Value>)) -> Val
		{
			f.upgrade().expect("map upgrade").call(Val{pool: p.upgrade(), value: v})
		}
		let map_pool = pool.downgrade();
		let vals = iter
			.map(move |v| {
				thunk::Thunk::new(
					map_pool.clone(),
					(map_pool.clone(), f.value.clone(), v.value.clone()),
					&do_map)
			})
			.collect();

		list::List::of_vals(pool, vals)
	}


	fn reverse(&self) -> Val {
		let val = self.value()?.reverse();
		self.merge(val)
	}

	pub fn rec_ser<'a>(&self, visited: &'a mut Vec<*const Value>) -> SerializeVal<'a> {
		let selfp = &*self.deref() as *const Value;
		if visited.contains(&selfp) { panic!("Recursive structure detected."); }
		visited.push(selfp);

		SerializeVal {
			val: self.clone(),
			visited: std::cell::RefCell::new(visited),
		}
	}

	pub fn eval(&self) -> Result<Val,Val> {
		self.deref().eval()?;
		Ok(self.clone())
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		self.value().unwrap().eq(&*that.deref()).to_bool()
	}
}

impl std::fmt::Debug for Val {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.deref().fmt(f)
	}
}

impl std::ops::Try for Val {
	type Ok = Self;
	type Error = Self;

	fn from_ok(v: Self::Ok) -> Self { v }
	fn from_error(v: Self::Error) -> Self { v }
	fn into_result(self) -> Result<Self::Ok, Self::Error> {
		if self.is_err() { Err(self.clone()) } else { Ok(self.clone()) }
	}
}

pub struct SerializeVal<'a> {
	val: Val,
	visited: std::cell::RefCell<&'a mut Vec<*const Value>>,
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
		self.rec_ser(unsafe{ std::mem::transmute(&mut v) }).serialize(s)
	}
}

#[derive(PartialEq)]
pub enum Almost {
	ADict(String,Rc<Almost>),
	Dict(Vec<dict::AlmostDictElement>),
	Add(grammar::Loc, Box<Almost>, Box<Almost>),
	Sub(grammar::Loc, Box<Almost>, Box<Almost>),
	Call(grammar::Loc, Box<Almost>, Box<Almost>),
	Eq(Box<Almost>, Box<Almost>),
	Great(Box<Almost>, Box<Almost>),
	GreatEq(Box<Almost>, Box<Almost>),
	Less(Box<Almost>, Box<Almost>),
	LessEq(Box<Almost>, Box<Almost>),
	Func(Rc<func::FuncData>),
	Index(grammar::Loc, Box<Almost>, Box<Almost>),
	List(Vec<Rc<Almost>>),
	Neg(grammar::Loc, Box<Almost>),
	Nil,
	Num(f64),
	Ref(grammar::Loc, String),
	StructRef(grammar::Loc, usize, String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl std::fmt::Debug for Almost {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match *self {
			Almost::Add(_, ref lhs, ref rhs) => write!(f, "({:?} + {:?})", lhs, rhs),
			Almost::Sub(_, ref lhs, ref rhs) => write!(f, "({:?} - {:?})", lhs, rhs),
			Almost::ADict(ref key, ref item) => {
				write!(f, "Adict{{{:?} = {:?}}}", key, item)
			}
			Almost::Dict(ref items) => {
				try!(writeln!(f, "{{"));
				for i in &**items {
					try!(writeln!(f, "\t{:?}", i));
				}
				write!(f, "}}")
			}
			Almost::Call(_, ref func, ref a) => write!(f, "({:?}:{:?})", func, a),
			Almost::Eq(ref l, ref r) => write!(f, "({:?} == {:?})", l, r),
			Almost::Great(ref l, ref r) => write!(f, "({:?} > {:?})", l, r),
			Almost::GreatEq(ref l, ref r) => write!(f, "({:?} >= {:?})", l, r),
			Almost::Less(ref l, ref r) => write!(f, "({:?} < {:?})", l, r),
			Almost::LessEq(ref l, ref r) => write!(f, "({:?} <= {:?})", l, r),
			Almost::Func(ref fd) => write!(f, "(->{:?} {:?})", fd.arg, fd.body),
			Almost::Index(_, ref obj, ref key) => write!(f, "{:?}.{:?}", obj, key),
			Almost::List(ref items) => {
				writeln!(f, "[")?;
				for item in items {
					writeln!(f, "\t{:?}", item)?;
				}
				write!(f, "]")
			}
			Almost::Neg(_, ref v) => write!(f, "-({:?})", v),
			Almost::Nil => write!(f, "nil"),
			Almost::Num(n) => write!(f, "{}", n),
			Almost::Ref(_, ref id) => write!(f, "Ref({})", format_key(id)),
			Almost::StructRef(_, d, ref key) => write!(f, "StructRef({})", format_ref(d, &key)),
			Almost::Str(ref parts) => {
				try!(write!(f, "Str(\""));
				for part in parts {
					match *part {
						StringPart::Exp(ref s) => try!(write!(f, "${{{:?}}}", s)),
						StringPart::Lit(ref s) => try!(write!(f, "{}", escape_string_contents(&s))),
					}
				}
				write!(f, "\"))")
			}
			Almost::StrStatic(ref s) => {
				write!(f, "{}", escape_string(&s))
			}
		}
	}
}

pub trait Parent: std::fmt::Debug {
	fn structural_lookup(&self, depth: usize, key: &::dict::Key) -> ::Val;
}

pub fn eval(source: &str, doc: &str) -> Val {
	assert!(source.find('/').is_none(), "Non-file source can't have a path.");
	grammar::parse(source, doc.chars())
		.map_err(|e| err::Err::new(format!("Failed to parse string: {:?}", e)))
		.and_then(bytecode::compile_to_vec)
		.map(bytecode::eval)
		.unwrap_or_else(|e| e)
}

pub fn parse_file(path: &str) -> Result<::Almost,::grammar::ParseError> {
	let file = match std::fs::File::open(path) {
		Ok(file) => file,
		Err(e) => panic!("Failed to open {:?}: {:?}", path, e),
	};

	let mut err = None;
	let chars = file.chars()
		.filter_map(|r| r.map_err(|e| err = Some(e)).ok())
		.fuse();

	grammar::parse(path, chars)
}

pub fn eval_file(path: &str) -> Val {
	parse_file(path)
		.map_err(|e| err::Err::new(format!("Failed to parse {:?}: {:?}", path, e)))
		.and_then(bytecode::compile_to_vec)
		.map(bytecode::eval)
		.unwrap_or_else(|e| e)
}

pub fn hacky_parse_func(source: &str, name: String, doc: &str) -> Val
{
	assert!(source.find('/').is_none(), "Non-file source can't have a path.");

	grammar::parse(source, doc.chars())
		.map_err(|e| err::Err::new(format!("Failed to parse {:?}: {:?}", source, e)))
		.map(|ast| {
			::Almost::Func(Rc::new(func::FuncData{
				arg: func::Arg::One(name),
				body: ast,
			}))
		})
		.and_then(bytecode::compile_to_vec)
		.map(bytecode::eval)
		.unwrap_or_else(|e| e)
}

pub fn dump_ast(doc: &str) -> Result<(), grammar::ParseError> {
	let almost = try!(grammar::parse("", doc.chars()));
	println!("{:?}", almost);
	Ok(())
}

#[derive(Debug,PartialEq)]
pub enum StringPart { Lit(String), Exp(Almost) }

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

fn format_ref(depth: usize, ident: &str) -> String {
	".".repeat(depth+1) + ident
}

#[cfg(test)]
mod tests {
	use super::*;

	// #[test]
	// fn val_is_rc_sized() {
	// 	// Testing an implementation detail. A value should be one word.
	// 	// If more falls out of https://github.com/rust-lang/rfcs/issues/1230
	// 	// we can probably pack more types into the enum safely. Otherwise we
	// 	// can consider having a pointer and doing the checking ourselves unsafely.
	// 	assert_eq!(std::mem::size_of::<Rc<Value>>(), std::mem::size_of::<Val>());
	// }

	#[test]
	fn list() {
		assert!(eval("<str>", "[]").is_empty());
		let v = eval("<str>", "[0d29 0b1.1]");
		assert_eq!(v.index_int(0), Val::new_atomic(29.0));
		assert_eq!(v.index_int(1), Val::new_atomic(1.5));
	}

	#[test]
	fn ident() {
		assert_eq!(eval("<str>","{b = 4}.b"), Val::new_atomic(4.0));
	}

	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion() {
		eval("<str>" ,"{b = b}").index_str("b").get_num();
	}

	#[test]
	#[should_panic(expected="Dependency cycle detected.")]
	fn recursion_multi_step() {
		eval("<str>", "{
			a = b
			b = c
			c = d
			d = b
		}").index_str("a").get_num();
	}
}
