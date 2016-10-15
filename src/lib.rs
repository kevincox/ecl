#![feature(plugin)]
#![plugin(peg_syntax_ext)]
peg_file! grammar("grammar.rustpeg");

use std::cell::{RefCell};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::iter::Iterator;
use std::mem;

extern crate serde;

pub mod lines;

fn i_promise_this_will_stay_alive<T>(v: &T) -> &'static T {
	unsafe { mem::transmute(v) }
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
	ThunkRef(ThunkRef),
}

static NIL: Val = Val::Nil;

unsafe impl Sync for Val {  }

impl Val {
	fn get(&self) -> &Val {
		// println!("{:?}.get()", self);
		let r = match *self {
			Val::Thunk(ref t) => t.eval(),
			Val::ThunkRef(ref t) => t.eval(),
			Val::Ref(r) => r,
			ref other => return other,
		};
		// println!("{:?}.get() -> {:?}", self, r);
		r.get()
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
			&Val::List(ref l) => l.parent.lookup(key),
			other => panic!("Lookup in non-container {:?}", other),
		}
	}
	
	fn add(&self, that: &Val) -> Val {
		match (self.get(), that.get()) {
			(&Val::Num(l), &Val::Num(r)) => Val::Num(l + r),
			(l, r) => panic!("Don't know how to add {:?} and {:?}", l, r),
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
		match self.get() {
			&Val::Dict(ref d) => {
				let mut state = try!(s.serialize_map(Some(d.source.len())));
				d.eval(self);
				let prv = d.data.borrow();
				for (k, i) in &prv.order {
					if let DictVal::Pub(ref v) = prv.data[*i] {
						try!(s.serialize_map_key(&mut state, k));
						try!(s.serialize_map_value(&mut state, v));
					}
				}
				s.serialize_map_end(state)
			},
			&Val::List(ref l) => {
				let len = l.source.len();
				let mut state = try!(s.serialize_seq_fixed_size(len));
				for i in 0..len {
					try!(s.serialize_seq_elt(&mut state, l.index(self, i)));
				}
				s.serialize_seq_end(state)
			},
			&Val::Nil => s.serialize_none(),
			&Val::Num(n) => {
				s.serialize_f64(n)
			},
			other => panic!("Don't know how to serialize {:?}", other),
		}
	}
}

pub struct Almost(Box<Fn(&'static Val) -> Val>);

impl Almost {
	fn val<F: Fn(&'static Val) -> Val + 'static>(f: F) -> Almost {
		Almost(Box::new(f))
	}
	
	fn dict(items: Vec<AlmostDictElement>) -> Almost {
		Almost::val(move |p| {
			let items = i_promise_this_will_stay_alive(&items);
			Val::Dict(Dict {
				parent: p,
				source: &items[..],
				data: RefCell::new(DictData {
					data: Vec::with_capacity(items.len()),
					order: BTreeMap::new(),
				})
			})
		})
	}
	
	fn list(items: Vec<Almost>) -> Almost {
		Almost::val(move |p| {
			let items: &'static [Almost] = unsafe { mem::transmute(&items[..]) };
			Val::List(List {
				parent: p,
				data: RefCell::new((0..items.len()).map(|_| None).collect()),
				source: items,
			})
		})
	}
	
	fn ref_(id: String) -> Almost {
		Almost::val(move |p| {
			let id = i_promise_this_will_stay_alive(&id);
			ThunkRef::new(move || p.lookup(id))
		})
	}
	
	fn suffix(subject: Almost, suffix: Suffix) -> Almost {
		Almost::val(move |p| {
			let sub = subject.complete(p);
			let suffix = i_promise_this_will_stay_alive(&suffix);
			Thunk::new(move || {
				let sub = i_promise_this_will_stay_alive(&sub);
				suffix.call(p, sub)
			})
		})
	}
	
	fn complete(&self, p: &Val) -> Val {
		let p: &Val = unsafe { mem::transmute(p) };
		self.0(p)
	}
}

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
	Add(Almost),
	IndexIdent(String),
}

impl Suffix {
	fn call(&self, parent: &'static Val, subject: &'static Val) -> Val {
		match *self {
			Suffix::Add(ref a) => {
				let val = a.complete(parent);
				Thunk::new(move || {
					let val = i_promise_this_will_stay_alive(&val);
					subject.add(val)
				})
			},
			Suffix::IndexIdent(ref id) =>
				Val::Ref(i_promise_this_will_stay_alive(subject.index_str(&id))),
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
	
	fn eval(&self, value: &Val) {
		loop {
			let next = self.data.borrow().data.len();
			
			let source = match self.source.get(next) {
				Some(s) => s,
				None => return,
			};
			
			let (k, v) = source.complete(value);
			
			let mut prv = self.data.borrow_mut();
			prv.data.push(v);
			
			match prv.order.entry(k) {
				Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
				Entry::Vacant(e) => e.insert(next),
			};
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
			return write!(f, "{{}}")
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
	data: RefCell<Option<Box<Val>>>,
}

impl Thunk {
	fn new<F: Fn() -> Val + 'static>(code: F) -> Val {
		Val::Thunk(Thunk{ code: Box::new(code), data: RefCell::new(None) })
	}
	
	fn eval(&self) -> &Val {
		let mut data = self.data.borrow_mut();
		if data.is_none() {
			*data = Some(Box::new((self.code)()))
		}
		i_promise_this_will_stay_alive(data.as_ref().unwrap())
	}
}
impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl fmt::Debug for Thunk {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<code>")
	}
}


pub struct ThunkRef {
	code: Box<Fn() -> &'static Val>,
	data: RefCell<Option<&'static Val>>,
}

impl ThunkRef {
	fn new<F: Fn() -> &'static Val + 'static>(code: F) -> Val {
		Val::ThunkRef(ThunkRef{ code: Box::new(code), data: RefCell::new(None) })
	}
	
	fn eval(&self) -> &Val {
		let mut data = self.data.borrow_mut();
		if data.is_none() {
			*data = Some((self.code)())
		}
		
		i_promise_this_will_stay_alive(data.unwrap())
	}
}
impl PartialEq for ThunkRef {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl fmt::Debug for ThunkRef {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<code>")
	}
}

pub fn parse(doc: &str) -> Result<Val, grammar::ParseError> {
	let almost = try!(grammar::document(doc));
	Ok(Thunk::new(move || almost.complete(&NIL)))
}

#[cfg(test)]
mod tests {
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
		assert_eq!(*v.index_str("a"), Val::Num(4.0));
		assert_eq!(*v.index_str("b"), Val::Num(0.0));
		
		let v = parse("{a=4 b=a}").unwrap();
		assert_eq!(*v.index_str("a"), Val::Num(4.0));
		assert_eq!(*v.index_str("b"), Val::Num(4.0));
	}
	
	#[test]
	fn list() {
		assert!(parse("[]").unwrap().is_empty());
		let v = parse("[0d29 0b1.1]").unwrap();
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
		// assert_eq!(parse("{b = 4}.b").unwrap(), Val::Num(4.0));
	}
}
