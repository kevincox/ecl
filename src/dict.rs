use erased_serde;
use gc;
use gc::Gc;
use std;
use std::fmt;

#[derive(Clone,Debug,Trace)]
struct Source {
	parent_structual: ::Val,
	#[unsafe_ignore_trace]
	key: Key,
	almost: ::bytecode::Value,
}

#[derive(Clone,Debug,Eq,Ord,PartialEq,PartialOrd,Trace)]
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

#[derive(Trace)]
pub struct Dict {
	prv: gc::GcCell<DictData>,
}

#[derive(Trace)]
struct DictData {
	data: Vec<DictPair>,
	source: Vec<Source>,
}

#[derive(Debug,Trace)]
pub struct DictPair {
	pub key: Key,
	pub val: Gc<::thunk::Thunky>,
}

impl DictPair {
	fn val(&self) -> ::Val {
		self.val.eval()
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
				let val = ::thunk::bytecode(this_pstruct.clone(), item);
				prv.data.push(DictPair{key, val});
			}
		}

		this
	}

	pub fn new_adict(pstruct: ::Val, k: String, item: ::bytecode::Value) -> ::Val {
		let key = Key::Pub(k.clone());

		let val = ::thunk::bytecode(pstruct.clone(), item.clone());
		let data = vec![
			DictPair{key, val: val}];

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

	pub fn _set_val(&self, key: Key, val: Gc<::thunk::Thunky>) {
		let mut prv = self.prv.borrow_mut();
		match prv.data.binary_search_by(|pair| pair.key.cmp(&key)) {
			Ok(_) => unreachable!("_set_val() for duplicate key {:?}", key),
			Err(i) => prv.data.insert(i, DictPair{key, val}),
		}
	}

	pub fn index(&self, key: &Key) -> Option<::Val> {
		let prv = self.prv.borrow();
		prv.data.iter().find(|pair| pair.key == *key)
			.map(|pair| pair.val())
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

				let mut val = ::thunk::bytecode(pstruct, s.almost.clone());
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

pub fn override_(sup: Gc<::thunk::Thunky>, sub: Gc<::thunk::Thunky>) -> Gc<::thunk::Thunky> {
	const F: &Fn((Gc<::thunk::Thunky>, Gc<::thunk::Thunky>)) -> ::Val = &|(sup, sub)| {
		let sub = sub.eval();

		if let Some(sub_dict) = sub.downcast_ref::<Dict>() {
			let sup = sup.eval().annotate("overriding error value")?;
			if let Some(sup_dict) = sup.downcast_ref::<Dict>() {
				return sup_dict.call(sub_dict)
			}
		}

		return sub
	};

	::thunk::Thunk::new((sup, sub), F)
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
				Key::Pub(ref k) => write!(f, "{:?}={:?}", k, pair.val)?,
				Key::Local(id) => write!(f, "local {:?}={:?}", id, pair.val)?,
			}
		}

		write!(f, "}}")
	}
}

impl ::Value for Dict {
	fn type_str(&self) -> &'static str { "dict" }

	fn eval(&self) -> Result<(),::Val> {
		for pair in &self.prv.borrow().data {
			if pair.key.is_public() {
				pair.val().eval()?;
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

	fn index_str(&self, key: &str) -> ::Val {
		self.index(&Key::Pub(key.to_owned()))
			.unwrap_or_else(::nil::get)
	}

	fn structural_lookup(&self, depth: usize, key: &Key) -> ::Val {
		assert_eq!(depth, 0, "Dict.structural_lookup({:?}, {:?})", depth, key);
		match self.index(key) {
			Some(v) => v,
			None => ::err::Err::new(format!("No {:?} in {:?}", key, self)),
		}
	}

	fn call(&self, arg: ::Val) -> ::Val {
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
				.filter(|pair| pair.key.is_public())
				.filter_map(|pair| match pair.key {
					Key::Pub(ref s) => {
						let data = vec![
							::thunk::Thunk::new(s.clone(), &::Val::new),
							::thunk::shim(pair.val()),
						];
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
				s.erased_serialize_map_value(&mut state, &pair.val().rec_ser(visited))?;
			}
		}
		s.erased_serialize_map_end(state)
	}
}

impl ::SameOps for Dict { }

#[derive(Clone,Copy,Eq,Ord,PartialEq,PartialOrd)]
pub enum Visibility {
	Local, Pub
}

#[derive(PartialEq)]
pub struct AlmostDictElement {
	pub visibility: Visibility,
	pub key: String,
	pub val: ::Almost,
}

impl AlmostDictElement {
	pub fn local(key: String, val: ::Almost) -> Self {
		AlmostDictElement{visibility: Visibility::Local, key, val}
	}

	pub fn public(key: String, val: ::Almost) -> Self {
		AlmostDictElement{visibility: Visibility::Pub, key, val}
	}

	pub fn is_public(&self) -> bool {
		self.visibility == Visibility::Pub
	}

	pub fn sort_cmp(&self, that: &AlmostDictElement) -> std::cmp::Ordering {
		(self.visibility, &self.key).cmp(&(that.visibility, &that.key))
	}
}

impl fmt::Debug for AlmostDictElement {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let vis = match self.visibility {
			Visibility::Pub => "pub  ",
			Visibility::Local => "local",
		};
		write!(f, "{} {:?} = {:?}", vis, self.key, self.val)
	}
}

#[derive(Clone,Debug,Trace)]
pub struct ParentSplitter {
	pub parent: ::Val,
	pub grandparent: ::Val,
}

impl ::Value for ParentSplitter {
	fn type_str(&self) -> &'static str { "parentsplitter" }

	fn structural_lookup(&self, depth: usize, key: &Key) -> ::Val {
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
