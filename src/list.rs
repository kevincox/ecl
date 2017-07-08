extern crate erased_serde;

use std::fmt;

#[derive(Trace,PartialEq)]
pub struct List {
	data: Vec<::Val>,
}

impl List {
	pub fn new(p: ::Val, items: &[::Almost]) -> ::Val {
		::Val::new(List {
			data: items.iter().map(|a| a.complete(p.clone())).collect(),
		})
	}
}

impl ::Value for List {
	fn type_str(&self) -> &'static str { "list" }
	fn is_empty(&self) -> bool { self.data.is_empty() }
	fn len(&self) -> usize { self.data.len() }
	
	fn index_int(&self, k: usize) -> ::Val {
		self.data[k].clone()
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
	fn reverse(&self) -> ::Val {
		let mut data: Vec<_> = self.data.clone();
		data.reverse();
		::Val::new(List {
			data: data,
		})
	}
}

impl ::SameOps for List { }

impl fmt::Debug for List {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.data.fmt(f)
	}
}
