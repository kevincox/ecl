extern crate erased_serde;
extern crate gc;
extern crate serde;

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::rc::Rc;

use thunk::Thunk;

#[derive(Trace)]
pub struct Dict {
	parent_structural: ::Val,
	parent_lexical: ::Val,
	prv: gc::GcCell<DictData>,
	
	#[unsafe_ignore_trace]
	source: Vec<AlmostDictElement>,
}

// TODO: Switch from enum to struct with visibility member.
#[derive(Clone,Debug,PartialEq,Trace)]
pub struct DictVal {
	visible: bool,
	val: ::Val,
}

impl DictVal {
	pub fn private(v: ::Val) -> DictVal {
		DictVal{visible: false, val: v}
	}
	
	pub fn public(v: ::Val) -> DictVal {
		DictVal{visible: true, val: v}
	}
}

#[derive(Debug,PartialEq,Trace)]
struct DictUneval(::Val,DictVal);

#[derive(PartialEq)]
pub struct DictData {
	data: BTreeMap<String,DictVal>,
	unevaluated: Vec<(DictUneval)>,
}

unsafe impl gc::Trace for DictData {
	gc::custom_trace!(this, {
		for (_, v) in &this.data {
			mark(v);
		}
		mark(&this.unevaluated);
	});
}

impl Dict {
	pub fn new(plex: ::Val, pstruct: ::Val, items: &[AlmostDictElement]) -> ::Val {
		let this = ::Val::new(Dict{
			parent_lexical: plex,
			parent_structural: pstruct,
			prv: gc::GcCell::new(DictData {
				data: BTreeMap::new(),
				unevaluated: Vec::new(),
			}),
			source: items.into(),
		});
		{
			let dict = this.clone();
			let dict = dict.downcast_ref::<Dict>().unwrap();
			for item in items {
				match item.complete(this.clone(), this.clone()) {
					DictPair::Known(k, v) => {
						match dict.prv.borrow_mut().data.entry(k) {
							Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
							Entry::Vacant(e) => e.insert(v),
						};
					},
					DictPair::Unknown(k, v) => {
						dict.prv.borrow_mut().unevaluated.push(DictUneval(k, v));
					},
				}
			}
		}
		this
	}
	
	pub fn _set_val(&self, key: String, val: DictVal) {
		self.prv.borrow_mut().data.insert(key, val);
	}
	
	fn eval_next(&self) -> Option<(String,DictVal)> {
		let DictUneval(ref k, ref v) = match self.prv.borrow_mut().unevaluated.pop() {
			Some(t) => t,
			None => return None,
		};
		
		let strkey = k.to_string().get_str().unwrap().to_owned();
		let mut prv = self.prv.borrow_mut();
		match prv.data.entry(strkey) {
			Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
			Entry::Vacant(e) => { e.insert(v.clone()); },
		};
		
		Some((k.to_string().get_str().unwrap().to_owned(), v.clone()))
	}
	
	fn eval(&self) {
		while let Some(_) = self.eval_next() { }
	}
	
	fn index(&self, key: &str) -> Option<DictVal> {
		{
			let prv = self.prv.borrow();
			if let Some(v) = prv.data.get(key) {
				return Some(v.clone())
			}
			
			if prv.unevaluated.is_empty() {
				return None
			}
		}
		
		while let Some((k, v)) = self.eval_next() {
			if k == key {
				return Some(v)
			}
		}
		
		None
	}
	
	fn call(&self, this: ::Val, arg: ::Val, pstruct: ::Val) -> ::Val {
		let arg = arg.get();
		let that = match arg.downcast_ref::<Dict>() {
			Some(dict) => dict,
			None => return ::err::Err::new(format!("Can't call dict with {:?}", arg)),
		};
		
		let child = ::Val::new(Dict{
			parent_structural: pstruct,
			parent_lexical:
				::err::Err::new("Child dict doesn't have it's own lexical parent.".to_owned()),
			prv: gc::GcCell::new(DictData {
				data: BTreeMap::new(),
				unevaluated: Vec::new(),
			}),
			source: Vec::new(),
		});
		
		{
			let dict = child.clone();
			let ref dict = dict.downcast_ref::<Dict>().unwrap();
			
			let that_elements = that.source.iter().map(|e| (e, arg.clone()));
			let self_elements = self.source.iter().map(|e| (e, this.clone()));
			
			for (item, parent) in that_elements.chain(self_elements) {
				match item.complete(parent, child.clone()) {
					DictPair::Known(k, v) => match dict.prv.borrow_mut().data.entry(k) {
						Entry::Occupied(mut e) => {
							let old = v.val.get();
							match old.downcast_ref::<Dict>() {
								Some(d) => {
									let new = e.get().val.clone();
									e.insert(DictVal::public(d.call(old.clone(), new, child.clone())));
								}
								None => {},
							};
						},
						Entry::Vacant(e) => {
							e.insert(v);
						},
					},
					DictPair::Unknown(k, v) => {
						dict.prv.borrow_mut().unevaluated.push(DictUneval(k, v));
					},
				}
			}
		}
		
		child
	}
}

impl fmt::Debug for Dict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let prv = self.prv.borrow();
		
		if prv.data.is_empty() && prv.unevaluated.is_empty() {
			return write!(f, "{{}}")
		}
		
		try!(writeln!(f, "{{"));
		for (k, v) in &prv.data {
			if v.visible {
				try!(writeln!(f, "\t{:?}: {:?}", k, v));
			}
		}
		
		let unevaluated = prv.unevaluated.len();
		if unevaluated > 0 {
			try!(writeln!(f, "\t<{} unevaluated>", unevaluated))
		}
		try!(write!(f, "}}"));
		Ok(())
	}
}

impl ::Value for Dict {
	fn type_str(&self) -> &'static str { "dict" }
	
	fn len(&self) -> usize {
		let prv = self.prv.borrow();
		prv.data.len() + prv.unevaluated.len()
	}
	
	fn is_empty(&self) -> bool {
		// Check if any of our un-evaluated elements are "public"
		let prv = self.prv.borrow();
		prv.data.values().all(|e| !e.visible) && prv.unevaluated.iter().all(|e| !e.1.visible)
	}
	
	fn index_str(&self, key: &str) -> ::Val {
		match self.index(key) {
			Some(element) => {
				if element.visible {
					element.val.clone()
				} else {
					::err::Err::new(format!("Attempt to access private member {:?}", key))
				}
			},
			None => ::nil::get(),
		}
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		match self.index(key) {
			Some(element) => element.val.clone(),
			None => self.parent_lexical.lookup(key),
		}
	}
	
	fn structural_lookup(&self, depth: usize, key: &str) -> Option<::Val> {
		println!("lookup({}, {})", depth, key);
		let v = match depth {
			0 => match self.index(key) {
				Some(element) => Some(element.val.clone()),
				None => None
			},
			other => {
				println!("Escalate to: {:?} from {:?}", self.parent_structural, self);
				self.parent_structural.structural_lookup(other-1, key)
			},
		};
		println!("lookup({}, {}) -> {:?}", depth, key, v.clone().map(|v| v.get()));
		v
	}
	
	fn call(&self, this: ::Val, arg: ::Val) -> ::Val {
		self.call(this, arg, ::nil::get())
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		self.eval();
		
		let prv = self.prv.borrow();
		assert_eq!(prv.unevaluated.len(), 0);
		
		let mut state = try!(s.erased_serialize_map(Some(prv.data.len())));
		for (k, i) in &prv.data {
			if i.visible {
				try!(s.erased_serialize_map_key(&mut state, k));
				try!(s.erased_serialize_map_value(&mut state, &i.val.rec_ser(visited)));
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for Dict { }

#[derive(Clone)]
pub enum AlmostDictElement {
	Unknown(Rc<::Almost>, Rc<::Almost>),
	Known(String, Rc<::Almost>),
	Priv(String, Rc<::Almost>),
}

enum DictPair {
	Known(String,DictVal),
	Unknown(::Val,DictVal),
}

impl AlmostDictElement {
	fn complete(&self, plex: ::Val, pstruct: ::Val) -> DictPair {
		match self {
			&AlmostDictElement::Unknown(ref k, ref v) => {
				DictPair::Unknown(
					Thunk::lazy(plex.clone(), pstruct.clone(), k.clone()),
					DictVal::public(Thunk::lazy(plex, pstruct, v.clone())))
			},
			&AlmostDictElement::Known(ref k, ref v) => {
				DictPair::Known(k.to_owned(), DictVal::public(Thunk::lazy(plex, pstruct, v.clone())))
			},
			&AlmostDictElement::Priv(ref k, ref v) => {
				DictPair::Known(k.to_owned(), DictVal::private(Thunk::lazy(plex, pstruct, v.clone())))
			},
		}
	}
}

impl fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&AlmostDictElement::Unknown(ref k, ref v) => {
				write!(f, "pub   {:?} = {:?}", k, v)
			},
			&AlmostDictElement::Known(ref k, ref v) => {
				write!(f, "pub   {} = {:?}", k, v)
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
	pub fn new(p: ::Val, key: String, val: ::Val) -> ::Val {
		::Val::new(ADict {
			parent: p,
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

impl ::Value for ADict {
	fn type_str(&self) -> &'static str { "adict" }
	fn is_empty(&self) -> bool { false }
	
	fn index_str(&self, key: &str) -> ::Val {
		if self.key == key {
			self.val.clone()
		} else {
			::nil::get()
		}
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		if self.key == key {
			self.val.clone()
		} else {
			self.parent.lookup(key)
		}
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let mut state = try!(s.erased_serialize_map(Some(1)));
		try!(s.erased_serialize_map_key(&mut state, &self.key));
		try!(s.erased_serialize_map_value(&mut state, &self.val.rec_ser(visited)));
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for ADict { }

#[cfg(test)]
mod tests {
	use super::super::*;
	
	#[test]
	fn dict() {
		assert!(parse("<str>", "{}").unwrap().is_empty());
		
		let v = parse("<str>", "{a=4 b = 0}").unwrap();
		assert_eq!(v.index_str("a"), ::Val::new(4.0));
		assert_eq!(v.index_str("b"), ::Val::new(0.0));
		
		let v = parse("<str>", "{a=4 b=a}").unwrap();
		assert_eq!(v.index_str("a"), ::Val::new(4.0));
		assert_eq!(v.index_str("b"), ::Val::new(4.0));
	}
	
	#[test]
	fn dict_recurse_key() {
		assert_eq!(parse("<str>", "{\"${b}\"=5 b=\"a\"}.a"), Ok(::Val::new(5.0)));
	}
}
