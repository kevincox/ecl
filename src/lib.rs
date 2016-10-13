#![feature(plugin,fnbox)]
#![plugin(peg_syntax_ext)]
peg_file! grammar("grammar.rustpeg");

use std::boxed::FnBox;
use std::cell::{Ref, RefMut, RefCell, UnsafeCell};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::iter::Iterator;
use std::mem;
use std::ops;
use std::rc::Rc;

extern crate serde;

pub mod serde_lines;

fn map<T, F>(val: &mut T, f: F)
	where F: FnOnce(T) -> T
{
	unsafe {
		let old = mem::replace(val, mem::uninitialized());
		mem::forget(mem::replace(val, f(old)));
	}
}

#[derive(Debug)]
pub enum Val {
	Dict(Dict),
	List(List),
	Num(f64),
	Nil,
	Ref(&'static Val),
	Str(String),
	Thunk(Thunk),
}

static NIL: Val = Val::Nil;

unsafe impl Sync for Val {  }

impl Val {
	fn get(&self) -> &Val {
		println!("get() on {:?}", self);
		match *self {
			Val::Thunk(ref t) => t.eval().get(),
			Val::Ref(r) => r.get(),
			ref other => other
		}
	}
	
	fn get_str(&self) -> Option<&str> {
		match *self.get() {
			Val::Str(ref s) => Some(s),
			_ => None,
		}
	}
	
	pub fn is_empty(&self) -> bool {
		match *self.get() {
			Val::List(ref l) => l.source.is_empty(),
			Val::Dict(ref d) => {
				if d.source.is_empty() {
					true
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
	
	pub fn index(&self, k: Val) -> &Val {
		match *k.get() {
			Val::Num(ref n) => self.index_int(*n as usize),
			ref k => panic!("Can't index with {:?}", k),
		}
	}
	
	fn index_int(&self, k: usize) -> &Val {
		match &*self.get() {
			&Val::List(ref l) => {
				l.index(self, k)
			},
			this => panic!("Can't index {:?} with an int", this),
		}
	}
	
	pub fn index_str(&self, key: &str) -> &Val {
		let isdict = match *self.get() {
			Val::Dict(_) => true,
			_ => false,
		};
		
		if isdict {
			match self.dict_index(key) {
				Some(&DictVal::Pub(ref v)) => v,
				Some(&DictVal::Priv(ref v)) => {
					println!("WRN: Attempt to access private memeber {:?}", v);
					&NIL
				},
				None => &NIL,
			}
		} else {
			panic!("Can't index {:?} with a string", self);
		}
	}
	
	fn dict_index(&self, key: &str) -> Option<&DictVal> {
		if let Val::Dict(ref d) = *self.get() {
			d.index(self, key)
		} else {
			panic!("Expected dict got {:?}", self);
		}
	}
	
	fn lookup(&self, key: &str) -> &Val {
		match self.get() {
			&Val::Nil => self,
			&Val::Dict(ref d) => {
				match self.dict_index(key) {
					Some(&DictVal::Pub(ref v)) => v,
					Some(&DictVal::Priv(ref v)) => v,
					None => d.parent.lookup(key),
				}
			},
			other => panic!("Lookup in non-container {:?}", other),
		}
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		match (self.get(), that.get()) {
			(&Val::Dict(ref l), &Val::Dict(ref r)) => l == r,
			(&Val::Num(ref l), &Val::Num(ref r)) => l == r,
			(_, _) => false,
		}
	}
}

impl serde::Serialize for Val {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(),S::Error> {
		s.serialize_none()
	}
}

#[derive(Debug)]
pub enum Almost {
	Dict(Vec<AlmostDictElement>),
	// Ind(Box<FnBox(Val) -> Box<FnBox() -> Val>>),
	List(Vec<Almost>),
	Ref(String),
	Val(Val),
}

impl Almost {
	fn val(v: Val) -> Almost {
		Almost::Val(v)
	}
	
	// fn make<A,T>(l: A) -> Almost where
	// 	A: FnOnce(Val) -> T + 'static,
	// 	T: FnBox() -> Val + 'static
	// {
	// 	Almost::Ind(Box::new(move |p| {
	// 		Box::new(l(p)) as Box<FnBox() -> Val>
	// 	}))
	// }
	
	fn complete(&self, p: &Val) -> Val {
		match self {
			&Almost::Dict(ref sd) => {
				let sd: &Vec<AlmostDictElement> = unsafe { mem::transmute(sd) };
				Val::Dict(Dict {
					parent: unsafe { mem::transmute(p) },
					source: &sd[..],
					data: RefCell::new(DictData {
						data: Vec::with_capacity(sd.len()),
						order: BTreeMap::new(),
					})
				})
			},
			// Almost::Ind(almostfun) => {
			// 	Thunk::new(almostfun(p))
			// },
			&Almost::List(ref sd) => {
				let sd: &Vec<Almost> = unsafe { mem::transmute(sd) };
				Val::List(List {
					parent: unsafe { mem::transmute(p) },
					data: RefCell::new((0..sd.len()).map(|_| None).collect()),
					source: &sd[..],
				})
			},
			&Almost::Ref(ref n) => {
				let n: &String = unsafe { mem::transmute(n) };
				let p: &Val = unsafe { mem::transmute(p) };
				Thunk::new(Box::new(move || {
					Val::Ref(unsafe { mem::transmute(p.lookup(n)) } )
				}))
			},
			&Almost::Val(ref v) => Val::Ref(unsafe { mem::transmute(v) }),
		}
	}
}

#[derive(Debug)]
pub enum AlmostDictElement {
	Unknown(Almost,Almost),
	Known(String,Almost),
	Priv(String,Almost),
}

impl AlmostDictElement {
	fn complete(&self, p: &Val) -> (String, DictVal) {
		match self {
			&AlmostDictElement::Unknown(ref k, ref v) => {
				let k = k.complete(p);
				let k = k
					.get_str().expect("Dict index must be string")
					.to_owned();
				let v = v.complete(p.clone());
				(k, DictVal::Pub(v))
			},
			&AlmostDictElement::Known(ref k, ref v) => {
				(k.to_owned(), DictVal::Pub(v.complete(p)))
			},
			&AlmostDictElement::Priv(ref k, ref v) => (k.to_owned(), DictVal::Priv(v.complete(p))),
		}
	}
}

pub enum Suffix {
	IndexIdent(String),
}

impl Suffix {
	fn call(&self, _parent: &'static Val, subject: &'static Val) -> &Val {
		match self {
			&Suffix::IndexIdent(ref id) => subject.index_str(&id),
		}
	}
}

pub struct List {
	parent: &'static Val,
	data: RefCell<Vec<Option<Val>>>,
	source: &'static [Almost]
}

impl List {
	fn index(&self, val: &Val, key: usize) -> &Val {
		if self.data.borrow()[key].is_none() {
			let mut l = self.data.borrow_mut();
			l[key] = Some(self.source[key].complete(val));
			l[key].as_ref().unwrap();
		}
		
		// I promise I won't modify/delete this value as long as I live.
		let r = self.data.borrow()[key].as_ref().unwrap() as *const Val;
		unsafe { &*r }
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

#[derive(Debug,PartialEq)]
enum DictVal { Pub(Val), Priv(Val) }

pub struct Dict {
	parent: &'static Val,
	source: &'static [AlmostDictElement],
	data: RefCell<DictData>,
}

#[derive(PartialEq)]
struct DictData {
	order: BTreeMap<String,usize>,
	data: Vec<DictVal>,
}

impl Dict {
	fn index(&self, value: &Val, key: &str) -> Option<&DictVal> {
		{
			let prv = self.data.borrow();
			if let Some(v) = prv.order.get(key) {
				// I promise I won't modify/delete this value as long as I live.
				let r = &prv.data[*v] as *const DictVal;
				return Some(unsafe { &*r })
			}
		}
		
		loop {
			let next = self.data.borrow().data.len();
			
			let source = match self.source.get(next) {
				Some(s) => s,
				None => return None,
			};
			
			let (k, v) = source.complete(value);
			
			let mut prv = self.data.borrow_mut();
			prv.data.push(v);
			
			match prv.order.entry(k) {
				Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
				Entry::Vacant(e) => {
					let found = e.key() == key;
					
					e.insert(next);
					
					if !found {
						continue
					}
				}
			}
			
			// I promise I won't modify/delete this value as long as I live.
			let r = &prv.data[next] as *const DictVal;
			return Some(unsafe { &*r })
		}
	}
}

impl PartialEq for Dict {
	fn eq(&self, _that: &Dict) -> bool {
		false
	}
}

impl fmt::Debug for Dict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.source.is_empty() {
			return writeln!(f, "{{}}")
		}
		
		let prv = self.data.borrow();
		
		try!(writeln!(f, "{{"));
		for (k, v) in &prv.order {
			match prv.data[*v] {
				DictVal::Pub(ref v) => try!(writeln!(f, "\t{:?}: {:?}", k, v)),
				_ => {},
			}
		}
		if !self.source.is_empty() {
			try!(writeln!(f, "\t<{} unevaluated>", self.source.len() - prv.data.len()))
		}
		try!(write!(f, "}}"));
		Ok(())
	}
}

pub struct Thunk {
	code: Box<Fn() -> Val>,
	data: UnsafeCell<Option<Box<Val>>>,
}

impl Thunk {
	fn new(code: Box<Fn() -> Val>) -> Val {
		Val::Thunk(Thunk{ code: code, data: UnsafeCell::new(None) })
	}
	
	fn eval(&self) -> &Val {
		unsafe {
			let data = self.data.get();
			if (*data).is_none() {
				*data = Some(Box::new((self.code)()))
			}
		}
		unsafe{&*self.data.get()}.as_ref().unwrap()
	}
}
impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

pub struct Context {
	almost: Almost,
	val: Val,
}

impl Context {
	fn new(doc: &str) -> Result<Context, grammar::ParseError> {
		let almost = try!(grammar::document(doc));
		let val = almost.complete(&NIL);
		Ok(Context { almost: almost, val: val })
	}
}

impl ops::Deref for Context {
	type Target = Val;
	
	fn deref(&self) -> &Self::Target {
		&self.val
	}
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
		assert!(Context::new("{}").unwrap().is_empty());
		let v = Context::new("{a=4 b = 0}").unwrap();
		assert_eq!(*v.index_str("a"), Val::Num(4.0));
		assert_eq!(*v.index_str("b"), Val::Num(0.0));
		
		let v = Context::new("{a=4 b=a}").unwrap();
		assert_eq!(*v.index_str("a"), Val::Num(4.0));
		assert_eq!(*v.index_str("b"), Val::Num(4.0));
	}
	
	#[test]
	fn list() {
		assert!(Context::new("[]").unwrap().is_empty());
		let v = Context::new("[0d29 0b1.1]").unwrap();
		assert_eq!(*v.index_int(0), Val::Num(29.0));
		assert_eq!(*v.index_int(1), Val::Num(1.5));
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
		// assert_eq!(Context::new("{b = 4}.b").unwrap(), Val::Num(4.0));
	}
	
	#[test]
	fn to_lines() {
// 		let mut out = Vec::new();
// 		Context::new(r###"
// 			{
// 				array = [ 1 2 3 { val = 1.3 } ]
// 				other = 2
// 				dict = {
// 					a = 1
// 					b = 2
// 					c = 3
// 				}
// 			}
// 		"###).unwrap().serialize(&mut serde_lines::Serializer::new(&mut out));
// 		assert_eq!(String::from_utf8(out).unwrap(), r###"array ARR [
// array.1 NUM 1
// array.2 NUM 2
// array.3 NUM 3
// array.4 DIC {
// array.4.val NUM 1.3
// dict.a NUM 1
// dict.b NUM 2
// dict.c NUM 3
// other NUM 2
// "###);
	}
}
