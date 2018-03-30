use erased_serde;

#[derive(Debug)]
pub struct Nil;

pub fn get() -> ::Val {
	::Val::new_inline(::Inline::Nil)
}

impl ::Value for Nil {
	fn type_str(&self) -> &'static str { "nil" }

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_none()
	}

	fn to_string(&self) -> ::Val {
		::Val::new_atomic("nil".to_owned())
	}

	fn to_bool(&self) -> bool {
		false
	}
}

impl ::SameOps for Nil {
	fn eq(&self, _: &Self) -> ::Val {
		::bool::get(true)
	}
}
