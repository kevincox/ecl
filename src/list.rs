extern crate erased_serde;

use std;
use std::rc::Rc;

pub struct List {
	pool: ::mem::WeakPoolHandle,
	data: Vec<Rc<::thunk::Thunky>>,
}

impl List {
	pub fn new(parent: Rc<::Parent>, items: Vec<::bytecode::Value>) -> ::Val {
		let pool = ::mem::PoolHandle::new();
		::Val::new(pool.clone(), List {
			pool: pool.downgrade(),
			data: items.iter()
				.map(|item| {
					::thunk::bytecode(
						parent.clone(),
						item.clone())
				})
				.collect(),
		})
	}

	pub fn of_vals(pool: ::mem::PoolHandle, data: Vec<Rc<::thunk::Thunky>>) -> ::Val {
		::Val::new(pool.clone(), List{pool: pool.downgrade(), data})
	}

	pub fn get(&self, i: usize) -> Option<::Val> {
		self.data.get(i).map(|i| i.eval(self.pool.upgrade()))
	}
}

impl ::Value for List {
	fn type_str(&self) -> &'static str { "list" }
	fn is_empty(&self) -> bool { self.data.is_empty() }
	fn len(&self) -> usize { self.data.len() }

	fn index_int(&self, k: usize) -> ::Val {
		self.data[k].eval(self.pool.upgrade())
	}

	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let len = self.data.len();
		let mut state = try!(s.erased_serialize_seq_fixed_size(len));
		for e in &self.data {
			s.erased_serialize_seq_elt(&mut state, &e.eval(self.pool.upgrade()).rec_ser(visited))?;
		}
		s.erased_serialize_seq_end(state)
	}

	fn iter<'a>(&'a self) -> Option<(::mem::PoolHandle, Box<Iterator<Item=::Val> + 'a>)> {
		let pool = self.pool.upgrade();
		Some((
			pool.clone(),
			Box::new(self.data.iter().map(move |v| v.eval(pool.clone())))))
	}

	fn reverse_iter<'a>(&'a self) -> Option<(::mem::PoolHandle, Box<Iterator<Item=::Val> + 'a>)> {
		let pool = self.pool.upgrade();
		Some((
			pool.clone(),
			Box::new(self.data.iter().rev().map(move |v| v.eval(pool.clone())))))
	}

	fn reverse(&self) -> ::Val {
		let mut data: Vec<_> = self.data.clone();
		data.reverse();
		::Val::new(self.pool.upgrade(), List {
			pool: self.pool.clone(),
			data: data,
		})
	}
}

impl ::SameOps for List {
	fn add(&self, that: &Self) -> ::Val {
		self.pool.merge(that.pool.upgrade());
		let mut data = Vec::with_capacity(self.data.len() + that.data.len());
		data.extend(self.data.iter().cloned());
		data.extend(that.data.iter().cloned());
		::Val::new(self.pool.upgrade(), List{pool: self.pool.clone(), data})
	}
}

impl std::fmt::Debug for List {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.data.fmt(f)
	}
}
