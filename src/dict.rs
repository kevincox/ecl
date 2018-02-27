extern crate erased_serde;
extern crate gc;
extern crate serde;

use std;
use std::fmt;
use std::rc::Rc;

#[derive(Clone,Debug,Trace)]
struct Source {
	parent_structual: ::Val,
	#[unsafe_ignore_trace]
	key: Key,
	almost: ::bytecode::Value,
}

#[derive(Trace)]
pub struct Dict {
	prv: gc::GcCell<DictData>,
}

#[derive(Trace)]
struct DictData {
	data: Vec<DictPair>,
	source: Vec<Source>,
}

#[derive(Clone,Debug,Eq,Ord,PartialEq,PartialOrd,Trace)]
pub enum Key {
	Local(usize),
	Pub(String),
}

impl Key {
	pub fn is_local(&self) -> bool {
		match *self {
			Key::Local(_) => true,
			_ => false,
		}
	}
}

#[derive(Clone,Debug,PartialEq,Trace)]
pub enum DictVal {
	Pub(::Val),
	Prv(::Val),
}

impl DictVal {
	fn val(&self) -> ::Val {
		match *self {
			DictVal::Pub(ref v) => v.clone(),
			DictVal::Prv(ref v) => v.clone(),
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
	pub fn new(pstruct: ::Val, items: Vec<(Key,::bytecode::Value)>) -> ::Val {
		let this = ::Val::new(Dict{
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
			
			for (key, item) in items {
				prv.source.push(Source{
					parent_structual: pstruct.clone(),
					key: key.clone(),
					almost: item.clone()
				});
				let val = ::thunk::Thunk::bytecode(this_pstruct.clone(), item);
				let dictval = if key.is_local() {
					DictVal::Prv(val)
				} else {
					DictVal::Pub(val)
				};
				prv.data.push(DictPair{key, val: dictval});
			}
		}
		
		this
	}
	
	pub fn new_adict(pstruct: ::Val, k: String, item: ::bytecode::Value) -> ::Val {
		let key = Key::Pub(k.clone());
		
		let val = ::thunk::Thunk::bytecode(pstruct.clone(), item.clone());
		let data = vec![
			DictPair{key, val: DictVal::Pub(val)}];
		
		let source = Source{
			parent_structual: pstruct,
			key: Key::Pub(k),
			almost: item,
		};
		
		::Val::new(Dict{
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
		let key = Key::Pub(key);
		let mut prv = self.prv.borrow_mut();
		match prv.data.binary_search_by(|pair| pair.key.cmp(&key)) {
			Ok(_) => unreachable!("_set_val() for duplicate key {:?}", key),
			Err(i) => prv.data.insert(i, DictPair{key, val}),
		}
	}
	
	fn index(&self, key: &Key) -> Option<DictVal> {
		let prv = self.prv.borrow();
		prv.data.iter().position(|pair| pair.key == *key)
			.map(|i| prv.data[i].val.clone())
		// prv.data.binary_search_by(|pair| pair.key.cmp(key))
		// 	.map(|i| prv.data[i].val.clone()).ok()
	}
	
	fn call(&self, that: &Dict) -> ::Val {
		let this = ::Val::new(Dict{
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
					(Some(l), Some(r)) => l.key.cmp(&r.key),
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
				
				let val = ::thunk::Thunk::bytecode(pstruct, s.almost.clone());
				if Some(&s.key) == data.last().map(|p| &p.key) {
					let inherited = override_(
						data.last().unwrap().val.val(),
						val.clone());
					data.last_mut().unwrap().val = DictVal::Pub(inherited);
				} else {
					let dictval = if s.key.is_local() {
						DictVal::Prv(val)
					} else {
						DictVal::Pub(val)
					};
					data.push(DictPair{key: s.key.clone(), val: dictval});
				}
				source.push(s);
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
			
			match pair.key {
				Key::Pub(ref k) => write!(f, "{:?}={:?}", k, pair.val.val())?,
				Key::Local(id) => write!(f, "local {:?}={:?}", id, pair.val.val())?,
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
		match self.index(&Key::Pub(key.to_owned())) {
			Some(DictVal::Pub(ref v)) => v.clone(),
			Some(_) => ::err::Err::new(format!("Attempt to access private member {:?}", key)),
			None => ::nil::get(),
		}
	}
	
	fn structural_lookup(&self, depth: usize, key: &Key) -> Option<::Val> {
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		let v = self.index(&key)
			.map(|element| match element {
				DictVal::Pub(ref v) => v.clone(),
				DictVal::Prv(ref v) => v.clone(),
			});
		v
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
				.filter_map(|pair| match pair.key {
					Key::Pub(ref s) => {
						let k = ::Val::new(s.clone());
						let data = vec![k, pair.val.val()];
						Some(::list::List::of_vals(data))
					}
					_ => None
				})))
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let prv = self.prv.borrow();
		
		let mut state = try!(s.erased_serialize_map(Some(prv.data.len())));
		for pair in &prv.data {
			if let Key::Pub(ref k) = pair.key {
				s.erased_serialize_map_key(&mut state, k)?;
				s.erased_serialize_map_value(&mut state, &pair.val.val().rec_ser(visited))?;
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for Dict { }

#[derive(Clone,PartialEq)]
pub struct AlmostDictElement {
	pub key: AlmostKey,
	pub val: Rc<::Almost>,
}

#[derive(Clone,Debug,Eq,Ord,PartialEq,PartialOrd)]
pub enum AlmostKey {
	Local(String),
	Pub(String),
}

impl AlmostKey {
	pub fn is_local(&self) -> bool {
		match *self {
			AlmostKey::Local(_) => true,
			_ => false,
		}
	}
}

#[derive(Debug,Trace)]
pub struct DictPair {
	pub key: Key,
	pub val: DictVal,
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
	
	fn structural_lookup(&self, depth: usize, key: &Key) -> Option<::Val> {
		match depth {
			0 => self.parent.structural_lookup(0, key),
			n => self.grandparent.structural_lookup(n-1, key),
		}
	}
}

impl ::SameOps for ParentSplitter { }

#[cfg(test)]
mod tests {
	use super::super::*;
	
	#[test]
	fn dict() {
		assert!(eval("<str>", "{}").is_empty());
		
		let v = eval("<str>", "{a=4 b = 0}");
		assert_eq!(v.index_str("a"), ::Val::new(4.0));
		assert_eq!(v.index_str("b"), ::Val::new(0.0));
		
		let v = eval("<str>", "{a=4 b=a}");
		assert_eq!(v.index_str("a"), ::Val::new(4.0));
		assert_eq!(v.index_str("b"), ::Val::new(4.0));
	}
}
