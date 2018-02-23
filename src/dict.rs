extern crate erased_serde;
extern crate gc;
extern crate serde;

use std;
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
	data: Vec<DictPair>,
	source: Vec<Source>,
}

#[derive(Clone,Eq,Ord,PartialEq,PartialOrd,Trace)]
pub struct Key {
	pub key: String,
	pub namespace: usize,
}

impl Key {
	pub fn new(key: String) -> Self { Key{namespace: 0, key: key} }
	pub fn local(ns: usize, key: String) -> Self { Key{namespace: ns, key: key} }
	fn is_local(&self) -> bool { self.namespace != 0 }
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
				data: Vec::with_capacity(items.len()),
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
				let pair = item.complete(this.clone(), this_pstruct.clone());
				if pair.key.is_local() {
					let redirect_key = Key::new(pair.key.key.clone());
					prv.data.push(DictPair{
						key: redirect_key,
						val: DictVal::Local(pair.key.namespace),
					});
				}
				prv.data.push(pair);
			}
		}
		
		this
	}
	
	pub fn new_adict(plex: ::Val, pstruct: ::Val, k: String, item: Rc<::Almost>) -> ::Val {
		let key = Key::new(k);
		
		let source = Source{
			parent_lexical: plex.clone(),
			parent_structual: pstruct.clone(),
			almost: AlmostDictElement{
				key: key.clone(),
				val: item.clone(),
			},
		};
		
		let data = vec![
			DictPair{key, val: DictVal::Pub(item.complete(plex.clone(), pstruct))}];
		
		::Val::new(Dict{
			parent_lexical: plex,
			prv: gc::GcCell::new(DictData{
				data: data,
				source: vec![source],
			}),
		})
	}
	
	fn source(&self) -> &[Source] {
		::i_promise_this_will_stay_alive(&*self.prv.borrow().source)
	}
	
	pub fn _set_val(&self, key: String, val: DictVal) {
		let key = Key::new(key);
		let mut prv = self.prv.borrow_mut();
		match prv.data.binary_search_by(|pair| pair.key.cmp(&key)) {
			Ok(_) => unreachable!("_set_val() for duplicate key {:?}", key),
			Err(i) => prv.data.insert(i, DictPair{key, val}),
		}
	}
	
	fn index(&self, key: &Key) -> Option<DictVal> {
		let prv = self.prv.borrow();
		prv.data.binary_search_by(|pair| pair.key.cmp(key))
			.map(|i| prv.data[i].val.clone()).ok()
	}
	
	fn call(&self, that: &Dict) -> ::Val {
		let this = ::Val::new(Dict{
			parent_lexical:
				::err::Err::new("Child dict doesn't have it's own lexical parent.".to_owned()),
			prv: gc::GcCell::new(DictData {
				data: Vec::new(),
				source: Vec::new(),
			}),
		});
		
		{
			let dict = this.clone();
			let ref dict = dict.downcast_ref::<Dict>().unwrap();
			let mut prv = dict.prv.borrow_mut();
			let DictData{ref mut source, ref mut data} = *prv;
			
			let mut left = self.source();
			let mut right = that.source();
			
			loop {
				let ord = match (left.first(), right.first()) {
					(Some(l), Some(r)) => l.almost.key.cmp(&r.almost.key),
					(Some(_), None) => std::cmp::Ordering::Less,
					(None, Some(_)) => std::cmp::Ordering::Greater,
					(None, None) => break,
				};
				
				let s = if ord == std::cmp::Ordering::Greater {
					let v = right[0].clone();
					right = &right[1..];
					v
				} else {
					let v = left[0].clone();
					left = &left[1..];
					v
				};
				
				let pstruct = ::Val::new(ParentSplitter{
					parent: this.clone(),
					grandparent: s.parent_structual.clone(),
				});
				
				let pair = s.almost.complete(s.parent_lexical.clone(), pstruct);
				if pair.key.is_local() {
					let redirect_key = Key::new(pair.key.key.clone());
					data.push(DictPair{
						key: redirect_key,
						val: DictVal::Local(pair.key.namespace),
					})
				}
				source.push(s);
				if Some(&pair.key) == data.last().map(|p| &p.key) {
					match pair.val {
						DictVal::Local(_) => unreachable!(),
						DictVal::Prv(_) => unreachable!(),
						DictVal::Pub(ref val) => {
							let inherited = override_(
								data.last().unwrap().val.val().unwrap(),
								val.clone());
							data.last_mut().unwrap().val = DictVal::Pub(inherited);
						}
					}
				} else {
					data.push(pair);
				}
			}
		}
		
		this
	}
}

pub fn override_(sup: ::Val, sub: ::Val) -> ::Val {
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
		for pair in &prv.data {
			write!(f, "{}", leader)?;
			leader = ' ';
			
			match pair.val {
				DictVal::Pub(ref v) => write!(f, "{:?}={:?}", pair.key.key, v)?,
				DictVal::Prv(ref v) => write!(f, "local {:?}={:?}", pair.key.key, v)?,
				DictVal::Local(ref _v) => {
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
		let prv = self.prv.borrow();
		prv.data.iter().all(|pair| !pair.val.public())
	}
	
	fn index_str(&self, key: &str) -> ::Val {
		match self.index(&Key::new(key.to_owned())) {
			Some(DictVal::Pub(ref v)) => v.clone(),
			Some(_) => ::err::Err::new(format!("Attempt to access private member {:?}", key)),
			None => ::nil::get(),
		}
	}
	
	fn structural_lookup(&self, depth: usize, key: &Key, private: bool) -> Option<::Val> {
		// eprintln!("structural_lookup({}, {:?}) in {:?}", depth, key, self);
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		let v = self.index(&key)
			.map(|element| match element {
				DictVal::Pub(ref v) => v.clone(),
				DictVal::Prv(ref v) => v.clone(),
				DictVal::Local(ns) => {
					if private {
						let mut key = key.clone();
						key.namespace = ns;
						self.index(&key).unwrap().val().unwrap()
					} else {
						::err::Err::new(format!("Attempt to access local {:?}", key.key))
					}
				}
			});
		// eprintln!("structural_lookup({}, {:?}) -> {:?}", depth, key, v);
		v
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
				.filter(|pair| pair.val.public())
				.map(|pair| {
					let k = ::Val::new(pair.key.key.clone());
					let data = vec![k, pair.val.val().unwrap()];
					::list::List::of_vals(data)
				})))
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let prv = self.prv.borrow();
		
		let mut state = try!(s.erased_serialize_map(Some(prv.data.len())));
		for pair in &prv.data {
			if let DictVal::Pub(ref v) = pair.val {
				s.erased_serialize_map_key(&mut state, &pair.key.key)?;
				s.erased_serialize_map_value(&mut state, &v.rec_ser(visited))?;
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for Dict { }

#[derive(Clone)]
pub struct AlmostDictElement {
	pub key: Key,
	pub val: Rc<::Almost>,
}

#[derive(Trace)]
struct DictPair {
	key: Key,
	val: DictVal,
}

impl DictPair {
	fn new(key: Key, val: DictVal) -> Self {
		DictPair{key, val}
	}
}

impl AlmostDictElement {
	fn complete(&self, plex: ::Val, pstruct: ::Val) -> DictPair {
		let val = Thunk::lazy(plex, pstruct, self.val.clone());
		let dictval = if self.key.is_local() { DictVal::Prv(val) } else { DictVal::Pub(val) };
		DictPair::new(self.key.clone(), dictval)
	}
}

impl fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let local = if self.key.is_local() { "local" } else { "pub  " };
		write!(f, "{} {:?} = {:?}", local, self.key, self.val)
	}
}

#[derive(Clone,Debug,Trace)]
pub struct ParentSplitter {
	pub parent: ::Val,
	pub grandparent: ::Val,
}

impl ::Value for ParentSplitter {
	fn type_str(&self) -> &'static str { "parentsplitter" }
	
	fn structural_lookup(&self, depth: usize, key: &Key, private: bool) -> Option<::Val> {
		// eprintln!("structural_lookup({}, {:?}) in {:?}", depth, key, self);
		match depth {
			0 => self.parent.structural_lookup(0, key, private),
			n => self.grandparent.structural_lookup(n-1, key, private),
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
