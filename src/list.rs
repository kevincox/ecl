extern crate erased_serde;

use std;
use gc::Gc;

#[derive(Trace,PartialEq)]
pub struct List {
	data: Vec<Gc<::thunk::Thunk>>,
}

impl List {
	pub fn new(pstruct: ::Val, items: Vec<::bytecode::Value>) -> ::Val {
		::Val::new(List {
			data: items.iter().map(|item|
				Gc::new(::thunk::Thunk::bytecode(pstruct.clone(), item.clone()))).collect(),
		})
	}
	
	pub fn of_vals(data: Vec<::thunk::Thunk>) -> ::Val {
		let data = data.into_iter()
			.map(Gc::new)
			.collect();
		::Val::new(List{data})
	}
	
	pub fn get(&self, i: usize) -> Option<::Val> {
		self.data.get(i).map(|i| i.eval())
	}
}

impl ::Value for List {
	fn type_str(&self) -> &'static str { "list" }
	fn is_empty(&self) -> bool { self.data.is_empty() }
	fn len(&self) -> usize { self.data.len() }
	
	fn index_int(&self, k: usize) -> ::Val {
		self.data[k].eval()
	}
	
	fn serialize(&self, visited: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		let len = self.data.len();
		let mut state = try!(s.erased_serialize_seq_fixed_size(len));
		for e in &self.data {
			try!(s.erased_serialize_seq_elt(&mut state, &e.eval().rec_ser(visited)));
		}
		s.erased_serialize_seq_end(state)
	}
	
	fn iter<'a>(&'a self) -> Option<Box<Iterator<Item=::Val> + 'a>> {
		Some(Box::new(self.data.iter().map(|v| v.eval())))
	}
	
	fn reverse_iter<'a>(&'a self) -> Option<Box<Iterator<Item=::Val> + 'a>> {
		Some(Box::new(self.data.iter().rev().map(|v| v.eval())))
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

impl std::fmt::Debug for List {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.data.fmt(f)
	}
}
