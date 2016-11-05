extern crate erased_serde;

impl ::Valu for String {
	fn type_str(&self) -> &'static str { "string" }
	
	fn get_str(&self) -> Option<&str> {
		Some(self)
	}
	
	fn serialize(&self,  _: &mut Vec<*const ::Valu>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self)
	}
	
	fn to_string(&self) -> String {
		self.clone()
	}
}

impl ::SameOps for String {
	fn add(&self, that: &Self) -> ::Val {
		::Val::new(self.clone() + that)
	}
	
	fn eq(&self, that: &Self) -> bool {
		self == that
	}
}
