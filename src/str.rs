use erased_serde;

impl ::Value for String {
	fn type_str(&self) -> &'static str { "string" }

	fn get_str(&self) -> Option<&str> {
		Some(self)
	}

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self)
	}
}

impl ::SameOps for String {
	fn add(&self, that: &Self) -> ::Val {
		::Val::new(self.clone() + that)
	}

	fn eq(&self, that: &Self) -> ::Val {
		::bool::get(self == that)
	}
}
