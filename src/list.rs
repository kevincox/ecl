extern crate erased_serde;

use std::fmt;
use std::rc::Rc;

use thunk::Thunk;

#[derive(Trace,PartialEq)]
pub struct List {
	data: Vec<::Val>,
}

impl List {
	pub fn new(plex: ::Val, pstruct: ::Val, items: &[Rc<::Almost>]) -> ::Val {
		::Val::new(List {
			data: items.iter().map(|a|
				Thunk::lazy(plex.clone(), pstruct.clone(), a.clone())).collect(),
		})
	}
	
	pub fn of_vals(data: Vec<::Val>) -> ::Val {
		::Val::new(List{data})
	}
}

impl ::Value for List {
	fn type_str(&self) -> &'static str { "list" }
	fn is_empty(&self) -> bool { self.data.is_empty() }
	fn len(&self) -> usize { self.data.len() }
	
	fn index_int(&self, k: usize) -> ::Val {
		self.data[k].clone()
	}
	
	fn to_slice(&self) -> &[::Val] {
		&self.data
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let len = self.data.len();
		let mut state = try!(s.erased_serialize_seq_fixed_size(len));
		for e in &self.data {
			try!(s.erased_serialize_seq_elt(&mut state, &e.rec_ser(visited)));
		}
		s.erased_serialize_seq_end(state)
	}
	
	fn iter<'a>(&'a self) -> Option<Box<Iterator<Item=::Val> + 'a>> {
		Some(Box::new(self.data.iter().cloned()))
	}
	
	fn reverse_iter<'a>(&'a self) -> Option<Box<Iterator<Item=::Val> + 'a>> {
		Some(Box::new(self.data.iter().rev().cloned()))
	}
	
	fn reverse(&self) -> ::Val {
		let mut data: Vec<_> = self.data.clone();
		data.reverse();
		::Val::new(List {
			data: data,
		})
	}
}

impl ::SameOps for List {
	fn add(&self, that: &Self) -> ::Val {
		let mut data = Vec::with_capacity(self.data.len() + that.data.len());
		data.extend(self.data.iter().cloned());
		data.extend(that.data.iter().cloned());
		::Val::new(List{data})
	}
}

impl fmt::Debug for List {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.data.fmt(f)
	}
}
