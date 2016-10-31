#![feature(plugin)]
#![feature(proc_macro)]
// #![plugin(afl_plugin)]
#![plugin(peg_syntax_ext)]
peg_file! grammar("grammar.rustpeg");

#[macro_use] extern crate gc;
#[macro_use] extern crate gc_derive;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate serde;

use gc::{Gc, GcCell};
use std::cell::{RefCell};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::iter::Iterator;
use std::mem;

pub mod lines;
pub use grammar::ParseError;

fn i_promise_this_will_stay_alive<T: ?Sized>(v: &T) -> &'static T {
	unsafe { mem::transmute(v) }
}

#[derive(Debug,Trace)]
pub enum Value {
	ADict(ADict),
	Bool(bool),
	Builtin(&'static Builtin),
	Dict(Dict),
	Func(Func),
	List(List),
	Num(f64),
	Nil,
	Str(String),
	Thunk(Thunk),
}

#[derive(Clone,Debug,Trace)]
pub struct Val(Gc<Value>);

impl Val {
	fn new(v: Value) -> Val {
		// println!("Allocating {:?}", v);
		Val(Gc::new(v))
	}
	
	fn get(&self) -> Val {
		// println!("{:?} ({:?}).get()", self, self as *const Val);
		let r = match **self {
			Value::Thunk(ref t) => t.eval().clone(),
			_ => return self.clone(),
		};
		// println!("{:?} ({:?}).get() -> {:?} ({:?})",
			// self, self as *const Val, r, r as *const Val);
		debug_assert!(&**self as *const Value != &*r as *const Value);
		let r = r.get();
		debug_assert!(&**self as *const Value != &*r as *const Value);
		r
	}
	
	fn get_str(&self) -> Option<&str> {
		match *self.get() {
			Value::Str(ref s) => Some(i_promise_this_will_stay_alive(s)),
			_ => None,
		}
	}
	
	pub fn is_empty(&self) -> bool {
		match *self.get() {
			Value::List(ref l) => l.data.is_empty(),
			Value::Dict(ref d) => {
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
	
	pub fn index(&self, k: Val) -> Val {
		match *k.get() {
			Value::Num(n) => self.index_int(n as usize),
			Value::Str(ref s) => self.index_str(s),
			ref k => panic!("Can't index {:?} with {:?}", self, k),
		}
	}
	
	fn index_int(&self, k: usize) -> Val {
		match &*self.get() {
			&Value::List(ref l) => {
				l.data[k].clone()
			},
			this => panic!("Can't index {:?} with an int", this),
		}
	}
	
	pub fn index_str(&self, key: &str) -> Val {
		// println!("Index {:?} in {:?}", key, self);
		match *self.get() {
			Value::ADict(ref d) => {
				if d.key == key {
					d.val.clone()
				} else {
					Val::new(Value::Nil)
				}
			},
			Value::Dict(ref d) => {
				match d.index(self.clone(), key) {
					Some(&DictVal::Pub(ref v)) => v.clone(),
					Some(&DictVal::Priv(ref v)) => {
						println!("WRN: Attempt to access private memeber {:?}", v);
						Val::new(Value::Nil)
					},
					None => Val::new(Value::Nil),
				}
			},
			ref other => panic!("Can't index {:?} with a string", other),
		}
	}
	
	fn lookup(&self, key: &str) -> Val {
		// println!("Lookup {:?} in {:?}", key, self);
		match *self.get() {
			Value::ADict(ref d) => {
				if d.key == key {
					d.val.clone()
				} else {
					d.parent.lookup(key)
				}
			},
			Value::Nil => {
				match key {
					"nil" => Val::new(Value::Nil),
					"true" => Val::new(Value::Bool(true)),
					"false" => Val::new(Value::Bool(false)),
					"reverse" => Val::new(Value::Builtin(&REVERSE)),
					other => panic!("Undefined variable {:?}", other),
				}
			},
			Value::Dict(ref d) => {
				match d.index(self.clone(), key) {
					Some(&DictVal::Pub(ref v)) => v.clone(),
					Some(&DictVal::Priv(ref v)) => v.clone(),
					None => d.parent.lookup(key),
				}
			},
			Value::List(ref l) => l.parent.lookup(key),
			ref other => panic!("Lookup in non-container {:?}", other),
		}
	}
	
	fn add(&self, that: Val) -> Val {
		Val::new(match (&*self.get(), &*that.get()) {
			(&Value::Num(l), &Value::Num(r)) => Value::Num(l + r),
			(&Value::Str(ref l), &Value::Str(ref r)) => Value::Str(l.clone() + &r),
			(l, r) => panic!("Don't know how to add {:?} and {:?}", l, r),
		})
	}
	
	fn call(&self, arg: Val) -> Val {
		match *self.get() {
			Value::Builtin(ref f) => {
				f.1(arg)
			},
			Value::Func(ref f) => {
				let f = i_promise_this_will_stay_alive(f);
				let scope = Val::new(Value::ADict(ADict {
					parent: f.parent.clone(),
					key: f.arg.to_owned(),
					val: arg.clone(),
				}));
				Thunk::new(vec![scope, f.parent.clone()], move |r| {
					f.body.complete(r[0].clone())
				})
			},
			ref other => panic!("Can't call {:?}", other),
		}
	}
	
	fn to_string(&self) -> String {
		match *self.get() {
			Value::Str(ref s) => s.clone(),
			Value::Num(n) => n.to_string(),
			ref other => panic!("Don't know how to turn {:?} into a string", other),
		}
	}
}

static REVERSE: Builtin = Builtin("reverse", &|v| {
						match *v.get() {
							Value::List(ref l) => {
								let mut data: Vec<_> = l.data.clone();
								data.reverse();
								Val::new(Value::List(List {
									parent: l.parent.clone(),
									data: data,
								}))
							}
							ref other => panic!("Can't reverse {:?}", other)
						}
					});

impl std::ops::Deref for Val {
	type Target = Value;
	
	fn deref(&self) -> &Self::Target {
		&*self.0
	}
}

impl PartialEq for Value {
	fn eq(&self, that: &Value) -> bool {
		match (self, that) {
			(&Value::Dict(ref l), &Value::Dict(ref r)) => l == r,
			(&Value::Num(l), &Value::Num(r)) => l == r,
			(_, _) => false,
		}
	}
}

impl PartialEq<Value> for Val {
	fn eq(&self, that: &Value) -> bool {
		*self.get() == *that
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		*self.get() == *that.get()
	}
}

impl serde::Serialize for Val {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(),S::Error> {
		match *self.get() {
			Value::ADict(ref d) => {
				let mut state = try!(s.serialize_map(Some(1)));
				try!(s.serialize_map_key(&mut state, &d.key));
				try!(s.serialize_map_value(&mut state, &d.val));
				s.serialize_map_end(state)
			},
			Value::Bool(b) => s.serialize_bool(b),
			Value::Dict(ref d) => {
				let mut state = try!(s.serialize_map(Some(d.source.len())));
				d.eval(self.clone());
				let prv = d.data.borrow();
				for (k, i) in &prv.order {
					if let DictVal::Pub(ref v) = prv.data[*i] {
						try!(s.serialize_map_key(&mut state, k));
						try!(s.serialize_map_value(&mut state, v));
					}
				}
				s.serialize_map_end(state)
			},
			Value::List(ref l) => {
				let len = l.data.len();
				let mut state = try!(s.serialize_seq_fixed_size(len));
				for e in &l.data {
					try!(s.serialize_seq_elt(&mut state, e));
				}
				s.serialize_seq_end(state)
			},
			Value::Nil => s.serialize_none(),
			Value::Num(n) => s.serialize_f64(n),
			Value::Str(ref str) => { s.serialize_str(str) },
			ref other => panic!("Don't know how to serialize {:?}", other),
		}
	}
}

pub struct Builtin(&'static str, &'static Fn(Val) -> Val);

unsafe impl gc::Trace for Builtin { unsafe_empty_trace!(); }
unsafe impl Sync for Builtin {  }

impl fmt::Debug for Builtin {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<builtin {:?}>", self.0)
	}
}

pub enum Almost {
	Dict(Vec<AlmostDictElement>),
	Expr(Box<Almost>, Vec<Suffix>),
	L(Box<Fn(Val) -> Val>),
	Num(f64),
	Ref(String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl Almost {
	fn val<F: Fn(Val) -> Val + 'static>(f: F) -> Almost {
		Almost::L(Box::new(f))
	}
	
	fn dict(items: Vec<AlmostDictElement>) -> Almost {
		Almost::Dict(items)
	}
	
	fn function(arg: String, body: Almost) -> Almost {
		Almost::val(move |p| {
			let arg = i_promise_this_will_stay_alive(&arg);
			let body = i_promise_this_will_stay_alive(&body);
			Val::new(Value::Func(Func { parent: p, arg: arg, body: body }))
		})
	}
	
	fn list(items: Vec<Almost>) -> Almost {
		Almost::val(move |p| {
			let items = i_promise_this_will_stay_alive(&items);
			List::new(p, items)
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
				let items = i_promise_this_will_stay_alive(items);
				Val::new(Value::Dict(Dict {
					parent: p,
					source: &items[..],
					data: GcCell::new(DictData {
						data: Vec::with_capacity(items.len()),
						order: BTreeMap::new(),
					})
				}))
			},
			Almost::Expr(ref subject, ref suffixes) => {
				let suffixes = i_promise_this_will_stay_alive(suffixes);
				let subject = i_promise_this_will_stay_alive(subject);
				Thunk::new(vec![p], move |r| {
					let subject = subject.complete(r[0].clone());
					suffixes.iter().fold(subject, |a, e| {
						e.call(r[0].clone(), a)
					})
				})
			},
			Almost::L(ref l) => l(p),
			Almost::Num(n) => Val::new(Value::Num(n)),
			Almost::Ref(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				Thunk::new(vec![p], move |r| r[0].lookup(id))
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
				Val::new(Value::Str(r))
			},
			Almost::StrStatic(ref s) => {
				Val::new(Value::Str(s.clone()))
			},
		}
	}
}

impl fmt::Debug for Almost {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Almost::Dict(ref items) => {
				try!(writeln!(f, "Dict({{"));
				for i in items {
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
			Almost::L(_) => write!(f, "<opaque>"),
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

pub enum AlmostDictElement {
	Auto(Vec<String>,Almost),
	Dyn(Vec<Almost>,Almost),
	Unknown(Almost,Almost),
	Known(String,Almost),
	Priv(String,Almost),
}

impl AlmostDictElement {
	fn complete(&self, p: Val) -> (String, DictVal) {
		match self {
			&AlmostDictElement::Auto(ref path, ref v) => {
				debug_assert!(!path.is_empty());
				(
					path[0].to_owned(),
					DictVal::Pub(Self::make_auto(p, &path[1..], v))
				)
			},
			&AlmostDictElement::Dyn(ref path, ref v) => {
				debug_assert!(!path.is_empty());
				let k = path[0].complete(p.clone())
					.get_str().expect("Dict index must be string")
					.to_owned();
				(
					k,
					DictVal::Pub(Self::make_auto_dyn(p, &path[1..], v))
				)
			},
			&AlmostDictElement::Unknown(ref k, ref v) => {
				let k = k.complete(p.clone());
				let k = k
					.get_str().expect("Dict index must be string")
					.to_owned();
				let v = v.complete(p);
				(k, DictVal::Pub(v))
			},
			&AlmostDictElement::Known(ref k, ref v) => {
				(k.to_owned(), DictVal::Pub(v.complete(p)))
			},
			&AlmostDictElement::Priv(ref k, ref v) => (k.to_owned(), DictVal::Priv(v.complete(p))),
		}
	}
	
	fn make_auto(p: Val, path: &[String], v: &Almost) -> Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			Val::new(Value::ADict(ADict {
				parent: p.clone(),
				key: path[0].to_owned(),
				val: Self::make_auto(p, &path[1..], v)
			}))
		}
	}
	
	fn make_auto_dyn(p: Val, path: &[Almost], v: &Almost) -> Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			let k = path[0].complete(p.clone());
			let k = k
				.get_str().expect("Dict index must be string")
				.to_owned();
			Val::new(Value::ADict(ADict {
				parent: p.clone(),
				key: k,
				val: Self::make_auto_dyn(p, &path[1..], v),
			}))
		}
	}
}

impl fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&AlmostDictElement::Auto(ref path, ref v) => {
				let mut sep = ' ';
				try!(write!(f, "pub  "));
				for e in path {
					try!(write!(f, "{}{}", sep, format_key(e)));
					sep = '.';
				}
				write!(f, " = {:?}", v)
			},
			&AlmostDictElement::Dyn(ref path, ref v) => {
				let mut sep = ' ';
				try!(write!(f, "pub  "));
				for e in path {
					try!(write!(f, "{}{:?}", sep, e));
					sep = '.';
				}
				write!(f, " = {:?}", v)
			},
			&AlmostDictElement::Unknown(ref k, ref v) => {
				write!(f, "pub   {:?} = {:?}", k, v)
			},
			&AlmostDictElement::Known(ref k, ref v) => {
				write!(f, "pub   {} = {:?}", format_key(k), v)
			},
			&AlmostDictElement::Priv(ref k, ref v) => {
				write!(f, "local {} = {:?}", format_key(k), v)
			},
		}
	}
}

pub enum Suffix {
	Add(Almost),
	Call(Almost),
	IndexExpr(Almost),
	IndexIdent(String),
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
			Suffix::IndexExpr(ref key) => {
				let k = key.complete(parent);
				Thunk::new(vec![subject, k], move |r| r[0].index(r[1].clone()))
			},
			Suffix::IndexIdent(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				Thunk::new(vec![subject], move |r| r[0].index_str(id))
			}
		}
	}
}

impl fmt::Debug for Suffix {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Suffix::Add(ref a) => write!(f, " + {:?}", a),
			Suffix::Call(ref a) => write!(f, ": {:?}", a),
			Suffix::IndexExpr(ref a) => write!(f, ".{:?}", a),
			Suffix::IndexIdent(ref s) => write!(f, ".{}", format_key(s)),
		}
	}
}

#[derive(Trace)]
pub struct List {
	parent: Val,
	data: Vec<Val>,
}

impl List {
	fn new(p: Val, items: &'static [Almost]) -> Val {
		Val::new(Value::List(List {
			parent: p.clone(),
			data: items.iter().map(|a| a.complete(p.clone())).collect(),
		}))
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

#[derive(Debug,PartialEq,Trace)]
enum DictVal { Pub(Val), Priv(Val) }

pub struct Dict {
	parent: Val,
	source: &'static [AlmostDictElement],
	data: GcCell<DictData>,
}

unsafe impl gc::Trace for Dict {
	custom_trace!(this, {
		mark(&this.parent);
		mark(&*this.data.borrow());
	});
}

#[derive(PartialEq,Trace)]
struct DictData {
	#[unsafe_ignore_trace]
	order: BTreeMap<String,usize>,
	data: Vec<DictVal>,
}

impl Dict {
	fn index(&self, value: Val, key: &str) -> Option<&DictVal> {
		{
			let prv = self.data.borrow();
			if let Some(v) = prv.order.get(key) {
				return Some(i_promise_this_will_stay_alive(&prv.data[*v]))
			}
		}
		
		loop {
			let next = self.data.borrow().data.len();
			
			let source = match self.source.get(next) {
				Some(s) => s,
				None => return None,
			};
			
			let (k, v) = source.complete(value.clone());
			
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
	
	fn eval(&self, value: Val) {
		loop {
			let next = self.data.borrow().data.len();
			
			let source = match self.source.get(next) {
				Some(s) => s,
				None => return,
			};
			
			let (k, v) = source.complete(value.clone());
			
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

#[derive(Trace)]
pub struct ADict {
	parent: Val,
	key: String,
	val: Val,
}

impl fmt::Debug for ADict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "ADict{{ {} = {:?} }}", format_key(&self.key), self.val)
	}
}

#[derive(Trace)]
pub struct Func {
	parent: Val,
	arg: &'static str,
	body: &'static Almost,
}

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} -> <body>", self.arg)
	}
}

// #[derive(Trace)]
pub struct Thunk {
	// #[unsafe_ignore_trace]
	code: Box<Fn(&Vec<Val>) -> Val>,
	refs: Vec<Val>,
	data: RefCell<Option<Val>>,
}

unsafe impl gc::Trace for Thunk {
	custom_trace!(this, {
		mark(&this.refs);
		unsafe { mark(&*this.data.as_ptr()); }
	});
}

impl Thunk {
	fn new<F: Fn(&Vec<Val>) -> Val + 'static>(refs: Vec<Val>, code: F) -> Val {
		Val::new(Value::Thunk(Thunk{ code: Box::new(code), refs: refs, data: RefCell::new(None) }))
	}
	
	fn eval(&self) -> Val {
		let mut data = self.data.borrow_mut();
		if data.is_none() {
			let new = (self.code)(&self.refs);
			unsafe { gc::Trace::unroot(&new.0) };
			*data = Some(new);
		}
		data.as_ref().unwrap().clone()
	}
}

impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl fmt::Debug for Thunk {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		// match *self.data.borrow() {
		// 	Some(ref v) => v.fmt(f),
		// 	None => write!(f, "<code>"),
		// }
		match self.data.try_borrow() {
			Ok(data) => match *data {
				Some(ref v) => v.fmt(f),
				None => write!(f, "<code>"),
			},
			Err(_) => write!(f, "<evaling>"),
		}
	}
}

pub fn parse(doc: &str) -> Result<Val, grammar::ParseError> {
	let almost = try!(grammar::document(doc));
	Ok(Thunk::new(vec![], move |_| almost.complete(Val::new(Value::Nil))))
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
		// assert_eq!(parse("{b = 4}.b").unwrap(), Value::Num(4.0));
	}
}
