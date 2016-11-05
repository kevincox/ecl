extern crate erased_serde;

impl ::Valu for bool {
	fn type_str(&self) -> &'static str { "bool" }
	
	fn serialize(&self,  _: &mut Vec<*const ::Valu>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_bool(*self)
	}
	
	fn to_string(&self) -> String {
		ToString::to_string(self)
	}
}

impl ::SameOps for bool {
	fn eq(&self, that: &Self) -> bool {
		*self == *that
	}
}
