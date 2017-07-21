extern crate erased_serde;

impl ::Value for f64 {
	fn type_str(&self) -> &'static str { "num" }
	
	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_f64(*self)
	}
	
	fn to_string(&self) -> ::Val {
		::Val::new(ToString::to_string(self))
	}
	
	fn get_num(&self) -> Option<f64> {
		Some(*self)
	}
}

impl ::SameOps for f64 {
	fn add(&self, that: &Self) -> ::Val {
		::Val::new(*self + *that)
	}
	
	fn eq(&self, that: &Self) -> ::Val {
		::bool::get(*self == *that)
	}
}
