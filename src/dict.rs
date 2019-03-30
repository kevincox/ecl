extern crate erased_serde;
extern crate serde;

use std;
use std::rc::Rc;

#[derive(Clone,Debug)]
struct Source {
	parent: Rc<crate::Parent>,
	key: Key,
	almost: crate::bytecode::Value,
}

#[derive(Clone,Debug,Eq,Ord,PartialEq,PartialOrd)]
pub enum Key {
	Local(usize),
	Pub(String),
}

impl Key {
	pub fn is_public(&self) -> bool {
		match *self {
			Key::Pub(_) => true,
			_ => false,
		}
	}
}

impl std::fmt::Display for Key {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match *self {
			Key::Local(id) => write!(f, "local #{}", id),
			Key::Pub(ref key) => write!(f, "{:?}", key),
		}
	}
}

pub struct Dict {
	pool: crate::mem::WeakPoolHandle,
	prv: std::cell::RefCell<DictData>,
}

struct DictData {
	data: Vec<DictPair>,
	source: Vec<Source>,
}

#[derive(Debug)]
pub struct DictPair {
	pub key: Key,
	pub val: Rc<crate::thunk::Thunky>,
}

impl DictPair {
	fn val(&self, pool: crate::mem::PoolHandle) -> crate::Val {
		self.val.eval(pool)
	}
}

impl Dict {
	pub fn new(parent: Rc<crate::Parent>, items: Vec<(Key,crate::bytecode::Value)>) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();

		let this = crate::Val::new(pool.clone(), Dict{
			pool: pool.downgrade(),
			prv: std::cell::RefCell::new(DictData{
				data: Vec::with_capacity(items.len()),
				source: Vec::with_capacity(items.len()),
			}),
		});

		let this_parent = Rc::new(ParentSplitter{
			parent: this.value.clone(),
			grandparent: Some(parent.clone()),
		});

		{
			let dict = this.downcast_ref::<Dict>().unwrap();
			let mut prv = dict.prv.borrow_mut();

			for (key, item) in items {
				prv.source.push(Source{
					parent: parent.clone(),
					key: key.clone(),
					almost: item.clone()
				});
				let val = crate::thunk::bytecode(this_parent.clone(), item);
				prv.data.push(DictPair{key, val});
			}
		}

		this
	}

	pub fn new_adict(parent: Rc<crate::Parent>, k: String, item: crate::bytecode::Value) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();

		let key = Key::Pub(k.clone());

		let val = crate::thunk::bytecode(parent.clone(), item.clone());
		let data = vec![
			DictPair{key, val: val}];

		let source = Source{
			parent: parent,
			key: Key::Pub(k),
			almost: item,
		};

		crate::Val::new(pool.clone(), Dict{
			pool: pool.downgrade(),
			prv: std::cell::RefCell::new(DictData{
				data: data,
				source: vec![source],
			}),
		})
	}

	fn source(&self) -> &[Source] {
		crate::i_promise_this_will_stay_alive(&*self.prv.borrow().source)
	}
	
	pub fn _set_val(&self, key: Key, val: Rc<crate::thunk::Thunky>) {
		let mut prv = self.prv.borrow_mut();
		match prv.data.binary_search_by(|pair| pair.key.cmp(&key)) {
			Ok(_) => unreachable!("_set_val() for duplicate key {:?}", key),
			Err(i) => prv.data.insert(i, DictPair{key, val}),
		}
	}

	pub fn index(&self, key: &Key) -> Option<crate::Val> {
		let prv = self.prv.borrow();
		prv.data.iter().find(|pair| pair.key == *key)
			.map(|pair| pair.val(self.pool.upgrade()))
		// prv.data.binary_search_by(|pair| pair.key.cmp(key))
		// 	.map(|i| prv.data[i].val.clone()).ok()
	}

	fn call(&self, that: &Dict) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();
		let this = crate::Val::new(pool.clone(), Dict{
			pool: pool.downgrade(),
			prv: std::cell::RefCell::new(DictData {
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

				let parent = Rc::new(ParentSplitter{
					parent: this.value.clone(),
					grandparent: Some(s.parent.clone()),
				});

				let mut val = crate::thunk::bytecode(
					parent,
					s.almost.clone());
				if Some(&s.key) == data.last().map(|p| &p.key) {
					let sup = data.pop().unwrap();
					val = override_(sup.val.clone(), val);
				}
				data.push(DictPair{key: s.key.clone(), val});
				source.push(s);
			}
		}

		this
	}
}

pub fn override_(
	sup: Rc<crate::thunk::Thunky>,
	sub: Rc<crate::thunk::Thunky>,
) -> Rc<crate::thunk::Thunky> {
	const F: &Fn((Rc<crate::thunk::Thunky>, Rc<crate::thunk::Thunky>)) -> crate::Val = &|(sup, sub)| {
		let sub = sub.eval(crate::mem::PoolHandle::new());

		if let Some(sub_dict) = sub.downcast_ref::<Dict>() {
			let sup = sup.eval(sub.pool.clone()).annotate("overriding error value")?;
			if let Some(sup_dict) = sup.downcast_ref::<Dict>() {
				return sup_dict.call(sub_dict)
			}
		}

		return sub
	};

	crate::thunk::Thunk::new((sup, sub), F)
}

impl std::fmt::Debug for Dict {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let prv = match self.prv.try_borrow() {
			Ok(prv) => prv,
			Err(e) => return write!(f, "{{<borrowed {:?}>}}", e),
		};

		if prv.data.is_empty() {
			return write!(f, "{{}}")
		}

		let mut leader = '{';
		for pair in &prv.data {
			write!(f, "{}", leader)?;
			leader = ' ';

			match pair.key {
				Key::Pub(ref k) => write!(f, "{:?}={:?}", k, pair.val)?,
				Key::Local(id) => write!(f, "local {:?}={:?}", id, pair.val)?,
			}
		}

		write!(f, "}}")
	}
}

impl crate::Value for Dict {
	fn type_str(&self) -> &'static str { "dict" }

	fn eval(&self) -> Result<(),crate::Val> {
		for pair in &self.prv.borrow().data {
			if pair.key.is_public() {
				pair.val(self.pool.upgrade()).eval()?;
			}
		}

		Ok(())
	}

	fn len(&self) -> usize {
		let prv = self.prv.borrow();
		prv.data.len()
	}

	fn is_empty(&self) -> bool {
		let prv = self.prv.borrow();
		prv.data.iter().all(|pair| !pair.key.is_public())
	}

	fn index_str(&self, key: &str) -> crate::Val {
		self.index(&Key::Pub(key.to_owned()))
			.unwrap_or_else(crate::nil::get)
	}

	fn structural_lookup(&self, depth: usize, key: &Key) -> crate::Val {
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		match self.index(key) {
			Some(v) => v,
			None => crate::err::Err::new(format!("No {:?} in {:?}", key, self)),
		}
	}

	fn call(&self, arg: crate::Val) -> crate::Val {
		match arg.downcast_ref::<Dict>() {
			Some(dict) => self.call(dict),
			None => crate::err::Err::new(format!("Can't call dict with {:?}", arg)),
		}
	}

	fn iter<'a>(&'a self) -> Option<(crate::mem::PoolHandle, Box<Iterator<Item=crate::Val> + 'a>)> {
		// This is fine because the dict has been fully evaluated.
		let data = crate::i_promise_this_will_stay_alive(&self.prv.borrow().data);

		let pool = self.pool.upgrade();
		Some((
			pool.clone(),
			Box::new(data
				.iter()
				.filter(|pair| pair.key.is_public())
				.filter_map(move |pair| match pair.key {
					Key::Pub(ref s) => {
						let data = vec![
							crate::thunk::shim(crate::Val::new(self.pool.upgrade(), s.clone())),
							crate::thunk::shim(pair.val(pool.clone())),
						];
						Some(crate::list::List::of_vals(self.pool.upgrade(), data))
					}
					_ => None
				}))))
	}

	fn serialize(&self, visited: &mut Vec<*const crate::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let prv = self.prv.borrow();

		let mut state = r#try!(s.erased_serialize_map(Some(prv.data.len())));
		for pair in &prv.data {
			if let Key::Pub(ref k) = pair.key {
				s.erased_serialize_map_key(&mut state, k)?;
				s.erased_serialize_map_value(&mut state,
					&pair.val(self.pool.upgrade()).rec_ser(visited))?;
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl crate::SameOps for Dict { }

#[derive(Clone,Copy,Eq,Ord,PartialEq,PartialOrd)]
pub enum Visibility {
	Assert, Local, Pub
}

#[derive(PartialEq)]
pub struct AlmostDictElement {
	pub visibility: Visibility,
	pub key: String,
	pub val: crate::Almost,
}

impl AlmostDictElement {
	pub fn assert(val: crate::Almost) -> Self {
		AlmostDictElement{visibility: Visibility::Assert, key: String::new(), val}
	}

	pub fn local(key: String, val: crate::Almost) -> Self {
		AlmostDictElement{visibility: Visibility::Local, key, val}
	}

	pub fn public(key: String, val: crate::Almost) -> Self {
		AlmostDictElement{visibility: Visibility::Pub, key, val}
	}

	pub fn is_element(&self) -> bool {
		match self.visibility {
			Visibility::Assert => false,
			Visibility::Local => true,
			Visibility::Pub => true,
		}
	}

	pub fn is_public(&self) -> bool {
		self.visibility == Visibility::Pub
	}

	pub fn sort_cmp(&self, that: &AlmostDictElement) -> std::cmp::Ordering {
		(self.visibility, &self.key).cmp(&(that.visibility, &that.key))
	}
}

impl std::fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		let vis = match self.visibility {
			Visibility::Assert => "assert",
			Visibility::Pub => "pub   ",
			Visibility::Local => "local ",
		};
		write!(f, "{} {:?} = {:?}", vis, self.key, self.val)
	}
}

#[derive(Clone,Debug)]
pub struct ParentSplitter {
	pub parent: crate::Inline,
	pub grandparent: Option<Rc<crate::Parent>>,
}

impl crate::Parent for ParentSplitter {
	fn structural_lookup(&self, depth: usize, key: &Key) -> crate::Val {
		match depth {
			0 => self.parent.structural_lookup(0, key),
			n => self.grandparent.as_ref().expect("grandparent access").structural_lookup(n-1, key),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::super::*;

	#[test]
	fn dict() {
		assert!(eval("<str>", "{}").is_empty());

		let v = eval("<str>", "{a=4 b = 0}");
		assert_eq!(v.index_str("a"), crate::Val::new_atomic(4.0));
		assert_eq!(v.index_str("b"), crate::Val::new_atomic(0.0));

		let v = eval("<str>", "{a=4 b=a}");
		assert_eq!(v.index_str("a"), crate::Val::new_atomic(4.0));
		assert_eq!(v.index_str("b"), crate::Val::new_atomic(4.0));
	}
}
