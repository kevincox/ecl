extern crate erased_serde;
extern crate gc;
extern crate serde;

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::rc;

#[derive(Trace)]
pub struct Dict {
	parent: ::Val,
	#[unsafe_ignore_trace]
	source: rc::Rc<Vec<AlmostDictElement>>,
	data: gc::GcCell<DictData>,
}

#[derive(Debug,PartialEq,Trace)]
enum DictVal { Pub(::Val), Priv(::Val) }

#[derive(PartialEq,Trace)]
pub struct DictData {
	this: ::Val,
	#[unsafe_ignore_trace]
	order: BTreeMap<String,usize>,
	data: Vec<DictVal>,
}

impl Dict {
	pub fn new(parent: ::Val, items: rc::Rc<Vec<AlmostDictElement>>) -> ::Val {
		let len = items.len();
		let r = ::Val::new(Dict {
			parent: parent,
			source: items,
			data: gc::GcCell::new(DictData {
				this: ::Val::new(::Value::Nil),
				data: Vec::with_capacity(len),
				order: BTreeMap::new(),
			})
		});
		r._set_self(r.clone());
		r
	}
	
	fn eval_next(&self) -> Option<(&str,&DictVal)> {
		let next = self.data.borrow().data.len();
		
		let source = match self.source.get(next) {
			Some(s) => s,
			None => return None,
		};
		
		let this = self.data.borrow().this.clone();
		
		// Insert a dummy value so that reentrance won't try to eval
		// this entry again.
		self.data.borrow_mut().data.push(DictVal::Priv(this.clone()));
		
		let (k, v) = source.complete(this);
		
		let mut prv = self.data.borrow_mut();
		prv.data[next] = v;
		
		let kref = match prv.order.entry(k.clone()) {
			Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
			Entry::Vacant(e) => {
				let kref = ::i_promise_this_will_stay_alive(e.key());
				e.insert(next);
				kref
			}
		};
		
		Some((kref, ::i_promise_this_will_stay_alive(&prv.data[next])))
	}
	
	fn index(&self, key: &str) -> Option<&DictVal> {
		{
			let prv = self.data.borrow();
			if let Some(v) = prv.order.get(key) {
				return Some(::i_promise_this_will_stay_alive(&prv.data[*v]))
			}
		}
		
		while let Some((k, v)) = self.eval_next() {
			if k == key {
				return Some(v)
			}
		}
		
		None
	}
	
	fn eval(&self) {
		while let Some(_) = self.eval_next() { }
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
		
		let unevaluated = self.source.len() - prv.order.len();
		if unevaluated > 0 {
			try!(writeln!(f, "\t<{} unevaluated>", unevaluated))
		}
		try!(write!(f, "}}"));
		Ok(())
	}
}

impl ::Valu for Dict {
	fn _set_self(&self, this: ::Val) {
		self.data.borrow_mut().this = this;
	}
	
	fn type_str(&self) -> &'static str { "dict" }
	
	fn is_empty(&self) -> bool {
		if self.source.is_empty() {
			true
		} else {
			// Check if any of our un-evaluated elements are "public"
			self.source.iter().all(|e| match e {
				&AlmostDictElement::Priv(_,_) => true,
				_ => false,
			})
		}
	}
	
	fn index_str(&self, key: &str) -> ::Val {
		match self.index(key) {
			Some(&DictVal::Pub(ref v)) => v.clone(),
			Some(&DictVal::Priv(ref v)) => {
				println!("WRN: Attempt to access private memeber {:?}", v);
				::Val::new(::Value::Nil)
			},
			None => ::Val::new(::Value::Nil),
		}
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		match self.index(key) {
			Some(&DictVal::Pub(ref v)) => v.clone(),
			Some(&DictVal::Priv(ref v)) => v.clone(),
			None => self.parent.lookup(key),
		}
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Valu>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let mut state = try!(s.erased_serialize_map(Some(self.source.len())));
		self.eval();
		let prv = self.data.borrow();
		for (k, i) in &prv.order {
			if let DictVal::Pub(ref v) = prv.data[*i] {
				try!(s.erased_serialize_map_key(&mut state, k));
				try!(s.erased_serialize_map_value(&mut state, &v.rec_ser(visited)));
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::ValuAdd for Dict { }

pub enum AlmostDictElement {
	Auto(Vec<String>,::Almost),
	Dyn(Vec<::Almost>,::Almost),
	Unknown(::Almost,::Almost),
	Known(String,::Almost),
	Priv(String,::Almost),
}

impl AlmostDictElement {
	fn complete(&self, p: ::Val) -> (String, DictVal) {
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
	
	fn make_auto(p: ::Val, path: &[String], v: &::Almost) -> ::Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			::Val::new(ADict {
				parent: p.clone(),
				key: path[0].to_owned(),
				val: Self::make_auto(p, &path[1..], v)
			})
		}
	}
	
	fn make_auto_dyn(p: ::Val, path: &[::Almost], v: &::Almost) -> ::Val {
		if path.is_empty() {
			v.complete(p)
		} else {
			let k = path[0].complete(p.clone());
			let k = k
				.get_str().expect("Dict index must be string")
				.to_owned();
			::Val::new(ADict {
				parent: p.clone(),
				key: k,
				val: Self::make_auto_dyn(p, &path[1..], v),
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
					try!(write!(f, "{}{}", sep, ::format_key(e)));
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
				write!(f, "pub   {} = {:?}", ::format_key(k), v)
			},
			&AlmostDictElement::Priv(ref k, ref v) => {
				write!(f, "local {} = {:?}", ::format_key(k), v)
			},
		}
	}
}

#[derive(Trace)]
pub struct ADict {
	parent: ::Val,
	key: String,
	val: ::Val,
}

impl ADict {
	pub fn new(parent: ::Val, key: String, val: ::Val) -> ::Val {
		::Val::new(ADict {
			parent: parent,
			key: key,
			val: val,
		})
	}
}

impl serde::Serialize for ADict {
	fn serialize<S: serde::Serializer>(&self, s: &mut S) -> Result<(),S::Error> {
		let mut state = try!(s.serialize_map(Some(1)));
		try!(s.serialize_map_key(&mut state, &self.key));
		try!(s.serialize_map_value(&mut state, &self.val));
		s.serialize_map_end(state)
	}
}

impl fmt::Debug for ADict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "ADict{{ {} = {:?} }}", ::format_key(&self.key), self.val)
	}
}

impl ::Valu for ADict {
	fn type_str(&self) -> &'static str { "adict" }
	fn is_empty(&self) -> bool { false }
	
	fn index_str(&self, key: &str) -> ::Val {
		if self.key == key {
			self.val.clone()
		} else {
			::Val::new(::Value::Nil)
		}
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		if self.key == key {
			self.val.clone()
		} else {
			self.parent.lookup(key)
		}
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Valu>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let mut state = try!(s.erased_serialize_map(Some(1)));
		try!(s.erased_serialize_map_key(&mut state, &self.key));
		try!(s.erased_serialize_map_value(&mut state, &self.val.rec_ser(visited)));
		s.erased_serialize_map_end(state)
	}
}

impl PartialEq for ADict {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl ::ValuAdd for ADict { }

#[cfg(test)]
mod tests {
	use super::super::*;
	
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
	fn dict_recurse_key() {
		assert_eq!(parse("{\"${b}\"=5 b=\"a\"}.a").unwrap(), Value::Num(5.0));
	}
}
