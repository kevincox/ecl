#![feature(plugin)]
// #![plugin(afl_plugin)]
#![plugin(peg_syntax_ext)]
peg_file! grammar("grammar.rustpeg");

#[macro_use] extern crate lazy_static;

use std::cell::{RefCell};
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::iter::Iterator;
use std::mem;

extern crate regex;
extern crate serde;

pub mod lines;
pub use grammar::ParseError;

fn i_promise_this_will_stay_alive<T>(v: &T) -> &'static T {
	unsafe { mem::transmute(v) }
}

#[derive(Debug)]
pub enum Val {
	ADict(ADict),
	Bool(bool),
	Builtin(Builtin),
	Dict(Dict),
	Func(Func),
	List(List),
	Num(f64),
	Nil,
	Ref(&'static Val),
	Str(String),
	Thunk(Thunk),
	ThunkRef(ThunkRef),
}

static NIL: Val = Val::Nil;
static TRUE: Val = Val::Bool(true);
static FALSE: Val = Val::Bool(false);

unsafe impl Sync for Val {  }

impl Val {
	fn get(&self) -> &Val {
		println!("{:?} ({:?}).get()", self, self as *const Val);
		let r = match *self {
			Val::Thunk(ref t) => t.eval(),
			Val::ThunkRef(ref t) => t.eval(),
			Val::Ref(r) => r,
			ref other => return other,
		};
		// println!("{:?} ({:?}).get() -> {:?} ({:?})",
			// self, self as *const Val, r, r as *const Val);
		debug_assert!(self as *const Val != r as *const Val);
		let r = r.get();
		debug_assert!(self as *const Val != r as *const Val);
		r
	}
	
	fn get_str(&self) -> Option<&str> {
		match *self.get() {
			Val::Str(ref s) => Some(s),
			_ => None,
		}
	}
	
	pub fn is_empty(&self) -> bool {
		match *self.get() {
			Val::List(ref l) => l.data.is_empty(),
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
	
	pub fn index(&self, k: &Val) -> &Val {
		match *k.get() {
			Val::Num(n) => self.index_int(n as usize),
			Val::Str(ref s) => self.index_str(s),
			ref k => panic!("Can't index {:?} with {:?}", self, k),
		}
	}
	
	fn index_int(&self, k: usize) -> &Val {
		match &*self.get() {
			&Val::List(ref l) => {
				&l.data[k]
			},
			this => panic!("Can't index {:?} with an int", this),
		}
	}
	
	pub fn index_str(&self, key: &str) -> &Val {
		println!("Index {:?} in {:?}", key, self);
		match *self.get() {
			Val::ADict(ref d) => {
				if d.key == key {
					&*d.val
				} else {
					&NIL
				}
			},
			Val::Dict(ref d) => {
				match d.index(i_promise_this_will_stay_alive(self), key) {
					Some(&DictVal::Pub(ref v)) => v,
					Some(&DictVal::Priv(ref v)) => {
						println!("WRN: Attempt to access private memeber {:?}", v);
						&NIL
					},
					None => &NIL,
				}
			},
			ref other => panic!("Can't index {:?} with a string", other),
		}
	}
	
	fn lookup(&self, key: &str) -> &Val {
		println!("Lookup {:?} in {:?}", key, self);
		match *self.get() {
			Val::ADict(ref d) => {
				if d.key == key {
					&*d.val
				} else {
					d.parent.lookup(key)
				}
			},
			Val::Nil => {
				match key {
					"nil" => &NIL,
					"true" => &TRUE,
					"false" => &FALSE,
					"reverse" => &REVERSE,
					other => panic!("Undefined variable {:?}", other),
				}
			},
			Val::Dict(ref d) => {
				match d.index(i_promise_this_will_stay_alive(self), key) {
					Some(&DictVal::Pub(ref v)) => v,
					Some(&DictVal::Priv(ref v)) => v,
					None => d.parent.lookup(key),
				}
			},
			Val::List(ref l) => l.parent.lookup(key),
			ref other => panic!("Lookup in non-container {:?}", other),
		}
	}
	
	fn add(&self, that: Val) -> Val {
		match (self.get(), that.get()) {
			(&Val::Num(l), &Val::Num(r)) => Val::Num(l + r),
			(&Val::Str(ref l), &Val::Str(ref r)) => Val::Str(l.clone() + &r),
			(l, r) => panic!("Don't know how to add {:?} and {:?}", l, r),
		}
	}
	
	fn call(&self, arg: Val) -> Val {
		match *self.get() {
			Val::Builtin(ref f) => {
				f.1(arg)
			},
			Val::Func(ref f) => {
				let f = i_promise_this_will_stay_alive(f);
				let scope = Val::ADict(ADict {
					parent: f.parent,
					key: f.arg.to_owned(),
					val: Box::new(arg),
				});
				Thunk::new(move || {
					let scope = i_promise_this_will_stay_alive(&scope);
					f.body.complete(scope)
				})
			},
			ref other => panic!("Can't call {:?}", other),
		}
	}
	
	fn to_string(&self) -> String {
		match *self.get() {
			Val::Str(ref s) => s.clone(),
			Val::Num(n) => n.to_string(),
			ref other => panic!("Don't know how to turn {:?} into a string", other),
		}
	}
}

impl PartialEq for Val {
	fn eq(&self, that: &Val) -> bool {
		match (self.get(), that.get()) {
			(&Val::Dict(ref l), &Val::Dict(ref r)) => l == r,
			(&Val::Num(l), &Val::Num(r)) => l == r,
			(_, _) => false,
		}
	}
}

impl serde::Serialize for Val {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(),S::Error> {
		let this = i_promise_this_will_stay_alive(self);
		match self.get() {
			&Val::ADict(ref d) => {
				let mut state = try!(s.serialize_map(Some(1)));
				try!(s.serialize_map_key(&mut state, &d.key));
				try!(s.serialize_map_value(&mut state, &d.val));
				s.serialize_map_end(state)
			},
			&Val::Bool(b) => s.serialize_bool(b),
			&Val::Dict(ref d) => {
				let mut state = try!(s.serialize_map(Some(d.source.len())));
				d.eval(this);
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
				let len = l.data.len();
				let mut state = try!(s.serialize_seq_fixed_size(len));
				for e in &l.data {
					try!(s.serialize_seq_elt(&mut state, e));
				}
				s.serialize_seq_end(state)
			},
			&Val::Nil => s.serialize_none(),
			&Val::Num(n) => s.serialize_f64(n),
			&Val::Str(ref str) => { s.serialize_str(str) },
			other => panic!("Don't know how to serialize {:?}", other),
		}
	}
}

pub struct Builtin(&'static str, &'static Fn(Val) -> Val);

impl fmt::Debug for Builtin {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "<builtin {:?}>", self.0)
	}
}

static REVERSE: Val = Val::Builtin(Builtin("reverse", &|v| {
	match *v.get() {
		Val::List(ref l) => {
			let l = i_promise_this_will_stay_alive(l); // TODO does it?
			let mut data: Vec<_> = l.data.iter().map(|v| Val::Ref(v)).collect();
			data.reverse();
			let p = i_promise_this_will_stay_alive(l.parent); // TODO does it?
			Val::List(List {
				parent: l.parent,
				data: data,
			})
		}
		ref other => panic!("Can't reverse {:?}", other)
	}
}));

pub enum Almost {
	Dict(Vec<AlmostDictElement>),
	Expr(Box<Almost>, Vec<Suffix>),
	L(Box<Fn(&'static Val) -> Val>),
	Num(f64),
	Ref(String),
	Str(Vec<StringPart>),
	StrStatic(String),
}

impl Almost {
	fn val<F: Fn(&'static Val) -> Val + 'static>(f: F) -> Almost {
		Almost::L(Box::new(f))
	}
	
	fn dict(items: Vec<AlmostDictElement>) -> Almost {
		Almost::Dict(items)
	}
	
	fn function(arg: String, body: Almost) -> Almost {
		Almost::val(move |p| {
			let arg = i_promise_this_will_stay_alive(&arg);
			let body = i_promise_this_will_stay_alive(&body);
			Val::Func(Func { parent: p, arg: arg, body: body })
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
	
	fn complete(&self, p: &'static Val) -> Val {
		match *self {
			Almost::Dict(ref items) => {
				let items = i_promise_this_will_stay_alive(items);
				Val::Dict(Dict {
					parent: p,
					source: &items[..],
					data: RefCell::new(DictData {
						data: Vec::with_capacity(items.len()),
						order: BTreeMap::new(),
					})
				})
			},
			Almost::Expr(ref subject, ref suffixes) => {
				let suffixes = i_promise_this_will_stay_alive(suffixes);
				let subject = i_promise_this_will_stay_alive(subject);
				Thunk::new(move || {
					let subject = subject.complete(p);
					suffixes.iter().fold(subject, |a, e| {
						e.call(p, a)
					})
				})
			},
			Almost::L(ref l) => l(p),
			Almost::Num(n) => Val::Num(n),
			Almost::Ref(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				ThunkRef::new(move || p.lookup(id))
			},
			Almost::Str(ref c) => {
				let mut r = String::new();
				for part in c {
					match part {
						&StringPart::Esc(s) => r.push(s),
						&StringPart::Exp(ref e) => r += &e.complete(p).to_string(),
						&StringPart::Lit(ref s) => r += &s,
					}
				}
				Val::Str(r)
			},
			Almost::StrStatic(ref s) => {
				Val::Str(s.clone())
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
	fn complete(&self, p: &'static Val) -> (String, DictVal) {
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
				let k = path[0].complete(p)
					.get_str().expect("Dict index must be string")
					.to_owned();
				(
					k,
					DictVal::Pub(Self::make_auto_dyn(p, &path[1..], v))
				)
			},
			&AlmostDictElement::Unknown(ref k, ref v) => {
				let k = k.complete(p);
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
	
	fn make_auto(p: &'static Val, path: &[String], v: &Almost) -> Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			Val::ADict(ADict {
				parent: p,
				key: path[0].to_owned(),
				val: Box::new(Self::make_auto(p, &path[1..], v))
			})
		}
	}
	
	fn make_auto_dyn(p: &'static Val, path: &[Almost], v: &Almost) -> Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			let k = path[0].complete(p);
			let k = k
				.get_str().expect("Dict index must be string")
				.to_owned();
			Val::ADict(ADict {
				parent: p,
				key: k,
				val: Box::new(Self::make_auto_dyn(p, &path[1..], v)),
			})
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
	fn call(&self, parent: &'static Val, subject: Val) -> Val {
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
				ThunkRef::new(move || {
					let k = i_promise_this_will_stay_alive(&k);
					i_promise_this_will_stay_alive(subject.index(k))
				})
			},
			Suffix::IndexIdent(ref id) => {
				let id = i_promise_this_will_stay_alive(id);
				ThunkRef::new(move || i_promise_this_will_stay_alive(subject.index_str(id)))
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

pub struct List {
	parent: &'static Val,
	data: Vec<Val>,
}

impl List {
	fn new(p: &'static Val, items: &'static [Almost]) -> Val {
		Val::List(List {
			parent: p,
			data: items.iter().map(|a| a.complete(p)).collect(),
		})
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
	fn index(&self, value: &'static Val, key: &str) -> Option<&DictVal> {
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
	
	fn eval(&self, value: &'static Val) {
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

pub struct ADict {
	parent: &'static Val,
	key: String,
	val: Box<Val>,
}

impl fmt::Debug for ADict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "ADict{{ {} = {:?} }}", format_key(&self.key), self.val)
	}
}

pub struct Func {
	parent: &'static Val,
	arg: &'static str,
	body: &'static Almost,
}

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} -> <body>", self.arg)
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
		match self.data.try_borrow() {
			Ok(data) => match *data {
				Some(ref v) => v.fmt(f),
				None => write!(f, "<code>"),
			},
			Err(_) => write!(f, "<evaling>"),
		}
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
	Ok(Thunk::new(move || almost.complete(&NIL)))
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
