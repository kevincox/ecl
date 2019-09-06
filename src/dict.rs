extern crate erased_serde;
extern crate serde;

use std;
use std::rc::Rc;

enum EvalState {
	Unchecked,
	Success,
	Error(crate::Val),
}

#[derive(Clone,Debug)]
pub enum Source {
	Assert{debug: usize, almost: crate::bytecode::Value},
	Entry{key: Key, almost: crate::bytecode::Value},
}

impl Source {
	fn sort_key<'a>(&'a self) -> impl Ord + 'a {
		match self {
			Source::Assert{..} => (0, &Key::Local(0)),
			Source::Entry{key, ..} => (1, key),
		}
	}
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
	state: EvalState,
	data: Vec<DictPair>,
	sources: Vec<(Rc<crate::Parent>, Source)>,
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
	pub fn new(parent: Rc<crate::Parent>, items: Vec<Source>) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();

		let this = crate::Val::new(pool.clone(), Dict{
			pool: pool.downgrade(),
			prv: std::cell::RefCell::new(DictData{
				state: EvalState::Unchecked,
				data: Vec::new(),
				sources: Vec::new(),
			}),
		});

		let this_parent = Rc::new(crate::Parent {
			parent: this.value.clone(),
			grandparent: Some(parent.clone()),
		});

		{
			let dict = this.downcast_ref::<Dict>().unwrap();
			let mut prv = dict.prv.borrow_mut();
			prv.sources.extend(items.into_iter().map(|item| (this_parent.clone(), item)));
		}

		this
	}

	pub fn new_adict(parent: Rc<crate::Parent>, k: String, item: crate::bytecode::Value) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();

		let key = Key::Pub(k.clone());

		let val = crate::thunk::bytecode(parent.clone(), item.clone());
		let data = vec![
			DictPair{key, val: val}];

		let source = Source::Entry {
			key: Key::Pub(k),
			almost: item,
		};

		crate::Val::new(pool.clone(), Dict{
			pool: pool.downgrade(),
			prv: std::cell::RefCell::new(DictData{
				state: EvalState::Success,
				data: data,
				sources: vec![(parent, source)],
			}),
		})
	}

	fn sources(&self) -> &[(Rc<crate::Parent>, Source)] {
		crate::i_promise_this_will_stay_alive(&*self.prv.borrow().sources)
	}
	
	fn eval_items(&self) -> Result<(), crate::Val> {
		match self.prv.borrow().state {
			EvalState::Unchecked => {}
			EvalState::Success => return Ok(()),
			EvalState::Error(ref e) => return Err(e.clone()),
		}

		{
			let DictData {
				ref mut state,
				ref mut data,
				ref sources,
			} = *self.prv.borrow_mut();

			for (parent, source) in sources {
				match &source {
					Source::Assert{..} => { }
					Source::Entry{key, almost} => {
						let mut val = crate::thunk::bytecode(
							parent.clone(),
							almost.clone());
						if Some(key) == data.last().map(|p| &p.key) {
							let sup = data.pop().unwrap();
							val = override_(sup.val.clone(), val);
						}
						data.push(DictPair{key: key.clone(), val});
					}
				}
			}

			// Not really. But we will convert to an error later if required.
			*state = EvalState::Success;
		}

		for (parent, source) in self.sources() {
			match source {
				Source::Assert{debug, almost} => {
					let val = almost.eval(parent.clone());
					if !val.to_bool()?.get_bool().unwrap() {
						let e = crate::err::Err::new_at(
							almost.module.loc(*debug),
							"Assertion failed".into())?;
						self.prv.borrow_mut().state = EvalState::Error(e.clone());
						return Err(e)
					}
				}
				Source::Entry{..} => {}
			}
		}

		Ok(())
	}

	pub fn _set_val(&self, key: Key, val: Rc<crate::thunk::Thunky>) {
		let mut prv = self.prv.borrow_mut();
		match prv.data.binary_search_by(|pair| pair.key.cmp(&key)) {
			Ok(_) => unreachable!("_set_val() for duplicate key {:?}", key),
			Err(i) => prv.data.insert(i, DictPair{key, val}),
		}
	}

	pub fn index(&self, key: &Key) -> Option<crate::Val> {
		self.eval_items().unwrap();

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
				state: EvalState::Unchecked,
				data: Vec::new(),
				sources: Vec::new(),
			}),
		});

		{
			let dict = this.clone();
			let ref dict = dict.downcast_ref::<Dict>().unwrap();
			let mut prv = dict.prv.borrow_mut();

			let mut left = self.sources();
			let mut right = that.sources();

			loop {
				let ord = match (left.first(), right.first()) {
					(Some(l), Some(r)) => l.1.sort_key().cmp(&r.1.sort_key()),
					(Some(_), None) => std::cmp::Ordering::Less,
					(None, Some(_)) => std::cmp::Ordering::Greater,
					(None, None) => break,
				};

				let (parent, source) = if ord == std::cmp::Ordering::Greater {
					let v = right[0].clone();
					right = &right[1..];
					v
				} else {
					let v = left[0].clone();
					left = &left[1..];
					v
				};

				let parent = Rc::new(crate::Parent {
					parent: this.value.clone(),
					grandparent: parent.grandparent.clone(),
				});

				prv.sources.push((parent, source));
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

		if let Ok(sub_dict) = sub.downcast_ref::<Dict>() {
			let sup = sup.eval(sub.pool.clone()).annotate("overriding error value")?;
			if let Ok(sup_dict) = sup.downcast_ref::<Dict>() {
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
		self.eval_items()?;
		for pair in &self.prv.borrow().data {
			if pair.key.is_public() {
				pair.val(self.pool.upgrade()).eval()?;
			}
		}

		Ok(())
	}

	fn len(&self) -> crate::Val {
		self.eval_items().unwrap();
		let prv = self.prv.borrow();
		let len = prv.data.len();
		crate::num::int(len)
	}

	fn is_empty(&self) -> crate::Val {
		self.eval_items().unwrap();
		let prv = self.prv.borrow();
		let is_empty = prv.data.iter().all(|pair| !pair.key.is_public());
		crate::bool::get(is_empty)
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
			Ok(dict) => self.call(dict),
			Err(_) => crate::err::Err::new(format!("Can't call dict with {:?}", arg)),
		}
	}

	fn iter<'a>(&'a self) -> Option<(crate::mem::PoolHandle, Box<Iterator<Item=crate::Val> + 'a>)> {
		self.eval_items().unwrap();

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
		self.eval_items().unwrap();
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

impl crate::SameOps for Dict {
	fn eq(&self, that: &Self) -> crate::Val {
		self.eval_items()?;
		that.eval_items()?;

		if self.prv.borrow().data.len() != that.prv.borrow().data.len() {
			return crate::bool::get(false)
		}

		crate::bool::get(self.cmp(that)? == std::cmp::Ordering::Equal)
	}

	fn cmp(&self, that: &Self) -> Result<std::cmp::Ordering,crate::Val> {
		self.eval_items()?;
		that.eval_items()?;

		// This is fine because the dict has been fully evaluated.
		let l = crate::i_promise_this_will_stay_alive(&self.prv.borrow().data);
		let r = crate::i_promise_this_will_stay_alive(&that.prv.borrow().data);

		let l = l.iter().filter_map(|pair| match pair.key {
				Key::Pub(ref name) => Some((name, &pair.val)),
				_ => None,
			});
		let r = r.iter().filter_map(|pair| match pair.key {
				Key::Pub(ref name) => Some((name, &pair.val)),
				_ => None,
			});

		let l_pool = self.pool.upgrade();
		let r_pool = that.pool.upgrade();

		for (l, r) in l.zip(r) {
			let ord = l.0.cmp(r.0);
			if ord != std::cmp::Ordering::Equal {
				return Ok(ord)
			}

			let ord = l.1.eval(l_pool.clone()).cmp(r.1.eval(r_pool.clone()))?;
			if ord != std::cmp::Ordering::Equal {
				return Ok(ord)
			}
		}

		return Ok(std::cmp::Ordering::Equal)
	}
}

#[derive(Clone,Copy,Eq,Ord,PartialEq,PartialOrd)]
pub enum Visibility {
	Assert, Local, Pub
}

#[derive(PartialEq)]
pub struct AlmostDictElement {
	pub visibility: Visibility,
	pub key: String,
	pub val: crate::Almost,
	pub loc: crate::grammar::Loc,
}

impl AlmostDictElement {
	pub fn assert(loc: crate::grammar::Loc, val: crate::Almost) -> Self {
		AlmostDictElement{loc, visibility: Visibility::Assert, key: String::new(), val}
	}

	pub fn local(loc: crate::grammar::Loc, key: String, val: crate::Almost) -> Self {
		AlmostDictElement{loc, visibility: Visibility::Local, key, val}
	}

	pub fn public(loc: crate::grammar::Loc, key: String, val: crate::Almost) -> Self {
		AlmostDictElement{loc, visibility: Visibility::Pub, key, val}
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

#[cfg(test)]
mod tests {
	use super::super::*;

	#[test]
	fn dict() {
		assert_eq!(eval("<str>", "{}").is_empty().get_bool(), Some(true));

		let v = eval("<str>", "{a=4 b = 0}");
		assert_eq!(v.is_empty().get_bool(), Some(false));
		assert_eq!(v.index_str("a").get_num(), Some(4.0));
		assert_eq!(v.index_str("b").get_num(), Some(0.0));

		let v = eval("<str>", "{a=4 b=a}");
		assert_eq!(v.is_empty().get_bool(), Some(false));
		assert_eq!(v.index_str("a").get_num(), Some(4.0));
		assert_eq!(v.index_str("b").get_num(), Some(4.0));
	}
}
