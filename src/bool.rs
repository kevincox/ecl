use erased_serde;

pub fn get(b: bool) -> crate::Val { crate::Val::new_inline(crate::Inline::Bool(b)) }

impl crate::Value for bool {
	fn type_str(&self) -> &'static str { "bool" }

	fn serialize(&self,  _: &mut Vec<*const crate::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_bool(*self)
	}

	fn to_string(&self) -> crate::Val {
		crate::Val::new_atomic(ToString::to_string(self))
	}

	fn to_bool(&self) -> bool {
		*self
	}

	fn get_bool(&self) -> Option<bool> {
		Some(*self)
	}
}

impl crate::SameOps for bool {
	fn eq(&self, that: &Self) -> crate::Val {
		get(*self == *that)
	}
}
