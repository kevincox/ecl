extern crate erased_serde;
extern crate gc;
extern crate serde;

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use std::rc::Rc;

use thunk::Thunk;

#[derive(Clone,Debug,Trace)]
struct Source {
	parent_lexical: ::Val,
	parent_structual: ::Val,
	#[unsafe_ignore_trace]
	almost: AlmostDictElement,
}

#[derive(Trace)]
pub struct Dict {
	parent_lexical: ::Val,
	prv: gc::GcCell<DictData>,
}

#[derive(Trace)]
struct DictData {
	data: BTreeMap<Key,DictVal>,
	source: Vec<Source>,
}

// unsafe impl gc::Trace for DictData {
// 	gc::custom_trace!(this, {
// 		for (_, v) in &this.data {
// 			mark(v);
// 		}
// 	});
// }


// TODO: Switch from enum to struct with visibility member.
#[derive(Clone,Eq,Ord,PartialEq,PartialOrd,Trace)]
pub struct Key {
	pub namespace: usize,
	pub key: String,
}

impl Key {
	pub fn new(key: String) -> Self { Key{namespace: 0, key: key} }
	pub fn local(ns: usize, key: String) -> Self { Key{namespace: ns, key: key} }
}

impl fmt::Debug for Key {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.namespace {
			0 => write!(f, "{:?}", self.key),
			_ => write!(f, "{}::{:?}", self.namespace, self.key),
		}
	}
}

#[derive(Clone,Debug,PartialEq,Trace)]
pub enum DictVal {
	Pub(::Val),
	Prv(::Val),
	Local(usize),
}

impl DictVal {
	fn val(&self) -> Option<::Val> {
		match *self {
			DictVal::Pub(ref v) => Some(v.clone()),
			DictVal::Prv(ref v) => Some(v.clone()),
			DictVal::Local(_) => None,
		}
	}
	
	fn public(&self) -> bool {
		match *self {
			DictVal::Pub(_) => true,
			_ => false,
		}
	}
}

impl Dict {
	pub fn new(plex: ::Val, pstruct: ::Val, items: &[AlmostDictElement]) -> ::Val {
		let this = ::Val::new(Dict{
			parent_lexical: plex,
			prv: gc::GcCell::new(DictData{
				data: BTreeMap::new(),
				source: Vec::with_capacity(items.len()),
			}),
		});
		
		let this_pstruct = ::Val::new(ParentSplitter{
			parent: this.clone(),
			grandparent: pstruct.clone(),
		});
		
		{
			let dict = this.downcast_ref::<Dict>().unwrap();
			let mut prv = dict.prv.borrow_mut();
			
			for item in items {
				prv.source.push(Source{
					parent_lexical: this.clone(),
					parent_structual: pstruct.clone(),
					almost: item.clone()
				});
				match item.complete(this.clone(), this_pstruct.clone()) {
					DictPair::Known(k, v) => {
						if k.namespace != 0 {
							let redirect_key = Key::new(k.key.clone());
							match prv.data.entry(redirect_key) {
								Entry::Occupied(e) =>
									panic!("Multiple entries for key {:?}", e.key()),
								Entry::Vacant(e) => e.insert(DictVal::Local(k.namespace)),
							};
						}
						
						match prv.data.entry(k) {
							Entry::Occupied(e) => panic!("Multiple entries for key {:?}", e.key()),
							Entry::Vacant(e) => e.insert(v),
						};
					},
				}
			}
		}
		
		this
	}
	
	fn source(&self) -> &[Source] {
		::i_promise_this_will_stay_alive(&*self.prv.borrow().source)
	}
	
	pub fn _set_val(&self, key: String, val: DictVal) {
		self.prv.borrow_mut().data.insert(Key::new(key), val);
	}
	
	fn index(&self, key: &Key) -> Option<DictVal> {
		let prv = self.prv.borrow();
		prv.data.get(key).map(|v| v.clone())
	}
	
	fn call(&self, that: &Dict) -> ::Val {
		let mut source = Vec::with_capacity(that.source().len() + self.source().len());
		source.extend(that.source().iter().cloned());
		source.extend(self.source().iter().cloned());
		
		let child = ::Val::new(Dict{
			parent_lexical:
				::err::Err::new("Child dict doesn't have it's own lexical parent.".to_owned()),
			prv: gc::GcCell::new(DictData {
				data: BTreeMap::new(),
				source: source,
			}),
		});
		
		{
			let dict = child.clone();
			let ref dict = dict.downcast_ref::<Dict>().unwrap();
			
			for s in dict.source() {
				let pstruct = ::Val::new(ParentSplitter{
					parent: child.clone(),
					grandparent: s.parent_structual.clone(),
				});
				match s.almost.complete(s.parent_lexical.clone(), pstruct.clone()) {
					DictPair::Known(k, v) => {
						match dict.prv.borrow_mut().data.entry(k) {
							Entry::Occupied(mut e) => {
								let sup_val = v.val().unwrap();
								let sub_val = e.get().val().unwrap();
								e.insert(DictVal::Pub(
									Thunk::new(vec![sup_val, sub_val], |r|
										override_(r[0].clone(), r[1].clone()))));
							},
							Entry::Vacant(e) => {
								e.insert(v);
							},
						}
					},
				}
			}
		}
		
		child
	}
}

fn override_(sup: ::Val, sub: ::Val) -> ::Val {
	let sub = sub.get();
	if let Some(sub_dict) = sub.downcast_ref::<Dict>() {
		let sup = sup.get()
			.annotate("overriding error value")?;
		if let Some(sup_dict) = sup.downcast_ref::<Dict>() {
			return sup_dict.call(sub_dict)
		}
	}
	return sub
}

impl fmt::Debug for Dict {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let prv = self.prv.borrow();
		
		if prv.data.is_empty() {
			return write!(f, "{{}}")
		}
		
		let mut leader = '{';
		for (k, v) in &prv.data {
			write!(f, "{}", leader)?;
			leader = ' ';
			
			match *v {
				DictVal::Pub(ref v) => write!(f, "{:?}={:?}", k, v)?,
				DictVal::Prv(ref v) => write!(f, "local {:?}={:?}", k, v)?,
				DictVal::Local(_v) => {
					// write!(f, "redir {:?}={:?}", k, _v)?;
				},
			}
		}
		
		write!(f, "}}")
	}
}

impl ::Value for Dict {
	fn type_str(&self) -> &'static str { "dict" }
	
	fn len(&self) -> usize {
		let prv = self.prv.borrow();
		prv.data.len()
	}
	
	fn is_empty(&self) -> bool {
		// Check if any of our un-evaluated elements are "public"
		let prv = self.prv.borrow();
		prv.data.values().all(|e| !e.public())
	}
	
	fn index_str(&self, key: &str) -> ::Val {
		match self.index(&Key::new(key.to_owned())) {
			Some(DictVal::Pub(ref v)) => v.clone(),
			Some(_) => ::err::Err::new(format!("Attempt to access private member {:?}", key)),
			None => ::nil::get(),
		}
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		match self.index(&Key::new(key.to_owned())) {
			Some(element) => element.val().unwrap(),
			None => self.parent_lexical.lookup(key),
		}
	}
	
	fn structural_lookup(&self, depth: usize, key: &Key) -> Option<::Val> {
		// eprintln!("structural_lookup({}, {:?}) in {:?}", depth, key, self);
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		let v = match self.index(key) {
			Some(element) => Some(element.val().unwrap()),
			None => None
		};
		// eprintln!("structural_lookup({}, {:?}) -> {:?}", depth, key, v);
		v
	}
	
	fn relative_lookup(&self, depth: usize, key: &str) -> Option<::Val> {
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		let mut key = Key::new(key.to_owned());
		// let dbg_key = key.clone();
		// eprintln!("Find {:?} in {:?}", dbg_key, self);
		self.index(&key)
			.map(|element| match element {
				DictVal::Pub(ref v) => v.clone(),
				DictVal::Prv(ref v) => v.clone(),
				DictVal::Local(ns) => {
					key.namespace = ns;
					self.index(&key).unwrap().val().unwrap()
				}
			})
	}
	
	fn find(&self, k: &str) -> (usize, Key, ::Val) {
		let mut key = Key::new(k.to_owned());
		// let dbg_key = key.clone();
		// eprintln!("Find {:?} in {:?}", dbg_key, self);
		let v = match self.index(&key) {
			Some(element) => match element {
				DictVal::Pub(ref v) => (0, key, v.clone()),
				DictVal::Prv(ref v) => (0, key, v.clone()),
				DictVal::Local(ns) => {
					key.namespace = ns;
					let v = self.index(&key).unwrap().val().unwrap();
					(0, key, v)
				}
			},
			None => {
				let (depth, k, v) = self.parent_lexical.deref().find(k);
				(depth+1, k, v)
			},
		};
		// eprintln!("Find {:?} in {:?} -> {:?}", dbg_key, self, v);
		return v
	}
	
	fn call(&self, arg: ::Val) -> ::Val {
		let arg = arg.get();
		match arg.downcast_ref::<Dict>() {
			Some(dict) => self.call(dict),
			None => ::err::Err::new(format!("Can't call dict with {:?}", arg)),
		}
	}
	
	fn iter<'a>(&'a self) -> Option<Box<Iterator<Item=::Val> + 'a>> {
		// This is fine because the dict has been fully evaluated.
		let data = ::i_promise_this_will_stay_alive(&self.prv.borrow().data);
		
		Some(Box::new(
			data
				.iter()
				.filter(|&(_, v)| v.public())
				.map(|(k, v)| {
					let k = ::Val::new(k.key.clone());
					let data = vec![k, v.val().unwrap()];
					::list::List::of_vals(data)
				})))
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let prv = self.prv.borrow();
		
		let mut state = try!(s.erased_serialize_map(Some(prv.data.len())));
		for (k, i) in &prv.data {
			if let DictVal::Pub(ref v) = *i {
				s.erased_serialize_map_key(&mut state, &k.key)?;
				s.erased_serialize_map_value(&mut state, &v.rec_ser(visited))?;
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for Dict { }

#[derive(Clone)]
pub enum AlmostDictElement {
	Known(Key, Rc<::Almost>),
	Priv(Key, Rc<::Almost>),
}

enum DictPair {
	Known(Key,DictVal),
}

impl AlmostDictElement {
	fn complete(&self, plex: ::Val, pstruct: ::Val) -> DictPair {
		match self {
			&AlmostDictElement::Known(ref k, ref v) => {
				DictPair::Known(k.to_owned(), DictVal::Pub(Thunk::lazy(plex, pstruct, v.clone())))
			},
			&AlmostDictElement::Priv(ref k, ref v) => {
				DictPair::Known(k.to_owned(), DictVal::Prv(Thunk::lazy(plex, pstruct, v.clone())))
			},
		}
	}
}

impl fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&AlmostDictElement::Known(ref k, ref v) => {
				write!(f, "pub   {:?} = {:?}", k, v)
			},
			&AlmostDictElement::Priv(ref k, ref v) => {
				write!(f, "local {:?} = {:?}", k, v)
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
	
	fn structural_lookup(&self, depth: usize, key: &Key) -> Option<::Val> {
		assert_eq!(depth, 0, "ADict.structural_lookup({:?}, {:?})", depth, key);
		if key.namespace == 0 && key.key == self.key {
			Some(self.val.clone())
		} else {
			None
		}
	}
	
	fn relative_lookup(&self, depth: usize, key: &str) -> Option<::Val> {
		assert_eq!(depth, 0, "ADict.relative_lookup({:?}, {:?})", depth, key);
		if key == self.key {
			Some(self.val.clone())
		} else {
			None
		}
	}
	
	fn find(&self, k: &str) -> (usize, Key, ::Val) {
		if self.key == k {
			(0, Key::new(k.to_owned()), self.val.clone())
		} else {
			self.parent.deref().find(k)
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

#[derive(Clone,Debug,Trace)]
pub struct ParentSplitter {
	pub parent: ::Val,
	pub grandparent: ::Val,
}

impl ::Value for ParentSplitter {
	fn type_str(&self) -> &'static str { "parentsplitter" }
	
	fn structural_lookup(&self, depth: usize, key: &Key) -> Option<::Val> {
		// eprintln!("structural_lookup({}, {:?}) in {:?}", depth, key, self);
		match depth {
			0 => self.parent.structural_lookup(0, key),
			n => self.grandparent.structural_lookup(n-1, key),
		}
	}
	
	fn relative_lookup(&self, depth: usize, key: &str) -> Option<::Val> {
		// eprintln!("relative_lookup({}, {:?}) in {:?}", depth, key, self);
		match depth {
			0 => self.parent.relative_lookup(0, key),
			n => self.grandparent.relative_lookup(n-1, key),
		}
	}
}

impl ::SameOps for ParentSplitter { }

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
}
