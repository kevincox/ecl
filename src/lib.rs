#![feature(plugin,fnbox)]
#![plugin(peg_syntax_ext)]
peg_file! grammar("grammar.rustpeg");

use std::boxed::FnBox;
use std::cell::{Ref, RefMut, RefCell};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::iter::Iterator;
use std::mem;
use std::rc::Rc;

extern crate serde;

pub mod serde_lines;

#[derive(Debug,PartialEq)]
pub enum Value {
	Dict(Dict),
	List(List),
	Num(f64),
	Nil,
	Ref(Val),
	Str(String),
	Thunk(Thunk),
}

#[derive(Clone)]
pub struct Val {
	v: Rc<RefCell<Value>>
}

fn map<T, F>(val: &mut T, f: F)
	where F: FnOnce(T) -> T
{
	unsafe {
		let old = mem::replace(val, mem::uninitialized());
		mem::forget(mem::replace(val, f(old)));
	}
}

impl Val {
	fn new(v: Value) -> Val { Val{ v: Rc::new(RefCell::new(v)) } }
	fn nil() -> Val { Self::new(Value::Nil) }
	
	fn get(&self) -> Ref<Value> {
		println!("get() on {:?}", self);
		if self.is_thunk() {
			map(&mut*self.v.borrow_mut(), |value| {
				if let Value::Thunk(t) = value {
					Value::Ref(t.eval())
				} else {
					unreachable!()
				}
			})
		}
		
		let r = self.v.borrow();
		match *r {
			Value::Ref(ref vr) => {
				// Rust, I promise that the lifetime of vr is as least as long
				// as self.
				let vr: &'static Val = unsafe { mem::transmute(vr as &Val) };
				vr.get()
			},
			Value::Thunk(_) => unreachable!(),
			_ => self.v.borrow(),
		}
	}
	
	fn borrow_mut(&self) -> RefMut<Value> {
		self.v.borrow_mut()
	}
	
	fn get_str(&self) -> Option<Ref<str>> {
		if let &Value::Str(_) = &*self.get() {
			return Some(Ref::map(self.get(), |s| {
				if let &Value::Str(ref s) = s {
					return s as &str
				}
				unreachable!();
			}))
		}
		None
	}
	
	fn is_thunk(&self) -> bool {
		// Can NOT use #get() because we don't want to evaluate the Thunk.
		match *self.v.borrow() {
			Value::Thunk(_) => true,
			_ => false
		}
	}
	
	pub fn is_empty(&self) -> bool {
		match *self.get() {
			Value::List(ref l) => l.data.is_empty(),
			Value::Dict(ref d) => {
				if !d.data.is_empty() {
					false
				} else {
					// Check if any of our un-evaluated elements are "public"
					d.source.iter().all(|e| match e {
						&AlmostDictElement::Priv(_,_) => true,
						_ => false,
					})
				}
			},
			_ => panic!("Don't know if {:?} is empty", self),
		}
	}
	
	pub fn index(&self, k: Val) -> Val {
		match *k.get() {
			Value::Num(ref n) => self.index_int(*n as usize),
			ref k => panic!("Can't index with {:?}", k),
		}
	}
	
	fn index_int(&self, k: usize) -> Val {
		match &*self.get() {
			&Value::List(ref l) => l.get(k),
			this => panic!("Can't index {:?} with an int", this),
		}
	}
	
	pub fn index_str(&self, key: &str) -> Val {
		let isdict = match *self.get() {
			Value::Dict(_) => true,
			_ => false,
		};
		
		if isdict {
			match self.dict_index(key) {
				Some(DictVal::Pub(v)) => return v.clone(),
				Some(DictVal::Priv(v)) => {
					println!("WRN: Attempt to access private memeber {:?}", v);
					Val::nil()
				},
				None => Val::nil(),
			}
		} else {
			panic!("Can't index {:?} with a string", self);
		}
	}
	
	fn dict_index(&self, key: &str) -> Option<DictVal> {
		if let Value::Dict(ref d) = *self.get() {
			if let Some(v) = d.data.get(key) {
				return Some(v.clone())
			}
		} else {
			panic!("Expected dict got {:?}", self);
		}
		
		loop {
			let source = if let Value::Dict(ref mut d) = *self.v.borrow_mut() {
				match d.source.pop() {
					Some(s) => s,
					None => return None,
				}
			} else {
				unreachable!()
			};
			
			let (k, v) = source.complete(self);
			
			if let Value::Dict(ref mut d) = *self.v.borrow_mut() {
				let found;
				match d.data.entry(k) {
					Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
					Entry::Vacant(e) => {
						found = e.key() == key;
						e.insert(v);
					}
				}
				if found {
					return Some(d.data.get(key).unwrap().clone())
				}
			} else {
				unreachable!();
			}
		}
	}
	
	fn lookup(&self, key: &str) -> Val {
		let next = match &*self.get() {
			&Value::Nil => Val::nil(),
			&Value::Dict(ref d) => {
				match self.dict_index(key) {
					Some(DictVal::Pub(v)) => return v,
					Some(DictVal::Priv(v)) => return v,
					None => d.parent.clone(),
				}
			},
			other => panic!("Lookup in non-container {:?}", other),
		};
		next.lookup(key)
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		*self.get() == *that.get()
	}
}

impl PartialEq<Value> for Val {
	fn eq(&self, that: &Value) -> bool {
		*self.get() == *that
	}
}

impl fmt::Debug for Val {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.v.try_borrow() {
			Ok(v) => v.fmt(f),
			Err(_) => write!(f, "<in use>"),
		}
	}
}

impl serde::Serialize for Val {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(),S::Error> {
		s.serialize_none()
	}
}

pub enum Almost {
	Dict(Vec<AlmostDictElement>),
	Ind(Box<FnBox(Val) -> Box<FnBox() -> Val>>),
	List(Vec<Almost>),
	Ref(String),
	Val(Val),
}

impl Almost {
	fn val(v: Value) -> Almost {
		Almost::Val(Val::new(v))
	}
	
	fn make<A,T>(l: A) -> Almost where
		A: FnOnce(Val) -> T + 'static,
		T: FnBox() -> Val + 'static
	{
		Almost::Ind(Box::new(move |p| {
			Box::new(l(p)) as Box<FnBox() -> Val>
		}))
	}
	
	fn complete(self, p: Val) -> Val {
		match self {
			Almost::Dict(sd) => {
				Val::new(Value::Dict(Dict {
					parent: p,
					data: BTreeMap::new(),
					source: sd,
				}))
			},
			Almost::Ind(almostfun) => {
				Thunk::new(almostfun(p))
			},
			Almost::List(ol) => {
				let nl = Val::new(Value::List(List {
					// parent: p,
					data: Vec::new(),
				}));
				let newdata = ol.into_iter()
					.map(|e| e.complete(nl.clone())).collect();
				match &mut *nl.borrow_mut() {
					&mut Value::List(ref mut l) => l.data = newdata,
					_ => unreachable!(),
				}
				nl
			},
			Almost::Ref(n) => {
				Thunk::new(Box::new(move || {
					p.lookup(&n)
				}))
			},
			Almost::Val(v) => v,
		}
	}
}

pub enum AlmostDictElement {
	Unknown(Almost,Almost),
	Known(String,Almost),
	Priv(String,Almost),
}

impl AlmostDictElement {
	fn complete(self, p: &Val) -> (String, DictVal) {
		match self {
			AlmostDictElement::Unknown(k, v) => {
				let k = k.complete(p.clone());
				let k = k
					.get_str().expect("Dict index must be string")
					.to_owned();
				let v = v.complete(p.clone());
				(k, DictVal::Pub(v))
			},
			AlmostDictElement::Known(k, v) => (k, DictVal::Pub(v.complete(p.clone()))),
			AlmostDictElement::Priv(k, v) => (k, DictVal::Priv(v.complete(p.clone()))),
		}
	}
}

pub enum Suffix {
	IndexIdent(String),
}

impl Suffix {
	fn call(self, _parent: Val, subject: Val) -> Val {
		match self {
			Suffix::IndexIdent(id) => subject.index_str(&id),
		}
	}
}

pub struct List {
	// parent: Val,
	data: Vec<Val>,
}

impl List {
	fn get(&self, i: usize) -> Val {
		self.data.get(i).expect("Element didn't exist.").clone()
	}
}

impl PartialEq for List {
	fn eq(&self, that: &List) -> bool {
		return self.data == that.data
	}
}

impl fmt::Debug for List {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.data.fmt(f)
	}
}

#[derive(PartialEq,Clone)]
enum DictVal { Pub(Val), Priv(Val) }

pub struct Dict {
	parent: Val,
	data: BTreeMap<String,DictVal>,
	source: Vec<AlmostDictElement>
}

impl PartialEq for Dict {
	fn eq(&self, that: &Dict) -> bool {
		return self.data == that.data
	}
}

impl fmt::Debug for Dict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.data.is_empty() && self.source.is_empty() {
			return write!(f, "{{}}")
		}
		
		try!(write!(f, "{{"));
		for (k, v) in &self.data {
			match v {
				&DictVal::Pub(ref v) => try!(write!(f, "\t{:?}: {:?}", k, v)),
				_ => {},
			}
		}
		if !self.source.is_empty() {
			try!(write!(f, "\t<{} unevaluated>", self.source.len()))
		}
		try!(write!(f, "}}"));
		Ok(())
	}
}

pub struct Thunk {
	code: Box<FnBox() -> Val>,
}

impl Thunk {
	fn new(code: Box<FnBox() -> Val>) -> Val {
		Val::new(Value::Thunk(Thunk{ code: code }))
	}
	
	fn eval(self) -> Val {
		(self.code)()
	}
}
impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

pub fn parse(doc: &str) -> Result<Val, grammar::ParseError> {
	let almost = try!(grammar::document(doc));
	Ok(almost.complete(Val::new(Value::Nil)))
}

impl fmt::Debug for Thunk {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<code>")
	}
}

#[cfg(test)]
mod tests {
	use serde::Serialize;
	
	use super::*;
	use super::grammar;
	
	macro_rules! with {
		( $p:pat = $x:expr, $b:block ) => {
			match $x {
				$p => $b,
				other => panic!("Expected {} got {:?}", stringify!($p), other),
			}
		}
	}
	
	#[test]
	fn dict() {
		assert!(parse("{}").unwrap().is_empty());
		let v = parse("{a=4 b = 0}").unwrap();
		assert_eq!(v.index_str("a"), Value::Num(4.0));
		assert_eq!(v.index_str("b"), Value::Num(0.0));
		
		let v = parse("{a=4 b=a}").unwrap();
		assert_eq!(v.index_str("a"), Value::Num(4.0));
		assert_eq!(v.index_str("b"), Value::Num(4.0));
	}
	
	#[test]
	fn list() {
		assert!(parse("[]").unwrap().is_empty());
		let v = parse("[0d29 0b1.1]").unwrap();
		assert_eq!(v.index_int(0), Value::Num(29.0));
		assert_eq!(v.index_int(1), Value::Num(1.5));
	}
	
	#[test]
	fn num_decimal() {
		assert_eq!(grammar::number("10"), Ok(10.0));
		assert_eq!(grammar::number("0d10"), Ok(10.0));
		assert_eq!(grammar::number("10.4"), Ok(10.4));
		assert_eq!(grammar::number("10.4e6"), Ok(10400000.0));
		assert_eq!(grammar::number("10.4e+6"), Ok(10400000.0));
		assert_eq!(grammar::number("10.4e-6"), Ok(0.0000104));
		assert_eq!(grammar::number("104_000__000_e-6_"), Ok(104.0));
		assert_eq!(grammar::number("1M"), Ok(1_000_000.0));
		assert_eq!(grammar::number("1ki"), Ok(1024.0));
		assert_eq!(grammar::number("4u"), Ok(0.000_004));
	}
	
	#[test]
	fn num_binary() {
		assert_eq!(grammar::number("0b10"), Ok(2.0));
		assert_eq!(grammar::number("0b10.1"), Ok(2.5));
		assert_eq!(grammar::number("0b10.1e6"), Ok(160.0));
		assert_eq!(grammar::number("0b1M"), Ok(1_000_000.0));
		assert_eq!(grammar::number("0b1u"), Ok(0.000_001));
	}
	
	#[test]
	fn num_hex() {
		assert_eq!(grammar::number("0x10"), Ok(16.0));
		assert_eq!(grammar::number("0x8.8"), Ok(8.5));
		assert_eq!(grammar::number("0x00.1"), Ok(0.0625));
	}
	
	#[test]
	fn ident() {
		assert_eq!(parse("{b = 4}.b").unwrap(), Value::Num(4.0));
	}
	
	#[test]
	fn to_lines() {
		let mut out = Vec::new();
		parse(r###"
			{
				array = [ 1 2 3 { val = 1.3 } ]
				other = 2
				dict = {
					a = 1
					b = 2
					c = 3
				}
			}
		"###).unwrap().serialize(&mut serde_lines::Serializer::new(&mut out));
		assert_eq!(String::from_utf8(out).unwrap(), r###"array ARR [
array.1 NUM 1
array.2 NUM 2
array.3 NUM 3
array.4 DIC {
array.4.val NUM 1.3
dict.a NUM 1
dict.b NUM 2
dict.c NUM 3
other NUM 2
"###);
	}
}
