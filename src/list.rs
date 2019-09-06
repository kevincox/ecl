extern crate erased_serde;

use std;
use std::rc::Rc;

pub struct List {
	pool: crate::mem::WeakPoolHandle,
	data: Vec<Rc<dyn crate::thunk::Thunky>>,
}

impl List {
	pub fn new(parent: Rc<crate::Parent>, items: Vec<crate::bytecode::Value>) -> crate::Val {
		let pool = crate::mem::PoolHandle::new();
		crate::Val::new(pool.clone(), List {
			pool: pool.downgrade(),
			data: items.iter()
				.map(|item| {
					crate::thunk::bytecode(
						parent.clone(),
						item.clone())
				})
				.collect(),
		})
	}

	pub fn of_vals(pool: crate::mem::PoolHandle, data: Vec<Rc<dyn crate::thunk::Thunky>>) -> crate::Val {
		crate::Val::new(pool.clone(), List{pool: pool.downgrade(), data})
	}

	pub fn get(&self, i: usize) -> Option<crate::Val> {
		self.data.get(i).map(|i| i.eval(self.pool.upgrade()))
	}

	fn iter<'a>(&'a self) -> (crate::mem::PoolHandle, Box<dyn Iterator<Item=crate::Val> + 'a>) {
		let pool = self.pool.upgrade();
		(pool.clone(), Box::new(self.data.iter().map(move |v| v.eval(pool.clone()))))
	}
}

impl crate::Value for List {
	fn type_str(&self) -> &'static str { "list" }
	fn is_empty(&self) -> crate::Val { crate::bool::get(self.data.is_empty()) }
	fn len(&self) -> crate::Val { crate::num::int(self.data.len()) }

	fn index_int(&self, k: usize) -> crate::Val {
		self.data[k].eval(self.pool.upgrade())
	}

	fn serialize(&self, visited: &mut Vec<*const dyn crate::Value>, s: &mut dyn erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let len = self.data.len();
		let mut state = r#try!(s.erased_serialize_seq_fixed_size(len));
		for e in &self.data {
			s.erased_serialize_seq_elt(&mut state, &e.eval(self.pool.upgrade()).rec_ser(visited))?;
		}
		s.erased_serialize_seq_end(state)
	}

	fn iter<'a>(&'a self) -> Option<(crate::mem::PoolHandle, Box<dyn Iterator<Item=crate::Val> + 'a>)> {
		Some(self.iter())
	}

	fn reverse_iter<'a>(&'a self) -> Option<(crate::mem::PoolHandle, Box<dyn Iterator<Item=crate::Val> + 'a>)> {
		let pool = self.pool.upgrade();
		Some((
			pool.clone(),
			Box::new(self.data.iter().rev().map(move |v| v.eval(pool.clone())))))
	}

	fn reverse(&self) -> crate::Val {
		let mut data: Vec<_> = self.data.clone();
		data.reverse();
		crate::Val::new(self.pool.upgrade(), List {
			pool: self.pool.clone(),
			data: data,
		})
	}
}

impl crate::SameOps for List {
	fn eq(&self, that: &Self) -> crate::Val {
		if self.data.len() != that.data.len() {
			return crate::bool::get(false)
		}

		for (l, r) in self.iter().1.zip(that.iter().1) {
			let ord = l.eq(r);
			if ord.get_bool() != Some(true) {
				return ord
			}
		}

		return crate::bool::get(true)
	}

	fn cmp(&self, that: &Self) -> Result<std::cmp::Ordering,crate::Val> {
		for (l, r) in self.iter().1.zip(that.iter().1) {
			let ord = l.cmp(r)?;
			if ord != std::cmp::Ordering::Equal {
				return Ok(ord)
			}
		}

		let (_l_pool, mut l) = self.iter();
		let (_r_pool, mut r) = that.iter();

		loop {
			match (l.next(), r.next()) {
				(None, Some(_)) => return Ok(std::cmp::Ordering::Less),
				(Some(l), Some(r)) => {
					let ord = l.cmp(r)?;
					if ord != std::cmp::Ordering::Equal {
						return Ok(ord)
					}
				}
				(Some(_), None) => return Ok(std::cmp::Ordering::Greater),
				(None, None) => return Ok(std::cmp::Ordering::Equal),
			}
		}
	}

	fn add(&self, that: &Self) -> crate::Val {
		self.pool.merge(that.pool.upgrade());
		let mut data = Vec::with_capacity(self.data.len() + that.data.len());
		data.extend(self.data.iter().cloned());
		data.extend(that.data.iter().cloned());
		crate::Val::new(self.pool.upgrade(), List{pool: self.pool.clone(), data})
	}
}

impl std::fmt::Debug for List {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.data.fmt(f)
	}
}
