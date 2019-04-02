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

	fn to_bool(&self) -> crate::Val {
		get(*self)
	}

	fn get_bool(&self) -> Option<bool> {
		Some(*self)
	}

	fn not(&self) -> crate::Val {
		get(!self)
	}
}

impl crate::SameOps for bool {
	fn cmp(&self, that: &Self) -> Result<std::cmp::Ordering,crate::Val> {
		Ok(std::cmp::Ord::cmp(self, that))
	}
}
