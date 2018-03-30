use erased_serde;

pub fn get(b: bool) -> ::Val { ::Val::new_inline(::Inline::Bool(b)) }

impl ::Value for bool {
	fn type_str(&self) -> &'static str { "bool" }

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_bool(*self)
	}

	fn to_string(&self) -> ::Val {
		::Val::new_atomic(ToString::to_string(self))
	}

	fn to_bool(&self) -> bool {
		*self
	}
}

impl ::SameOps for bool {
	fn eq(&self, that: &Self) -> ::Val {
		get(*self == *that)
	}
}
