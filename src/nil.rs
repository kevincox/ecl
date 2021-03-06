use erased_serde;

#[derive(Debug)]
pub struct Nil;

pub fn get() -> crate::Val {
	crate::Val::new_inline(crate::Inline::Nil)
}

impl crate::Value for Nil {
	fn type_str(&self) -> &'static str { "nil" }

	fn serialize(&self,  _: &mut Vec<*const dyn crate::Value>, s: &mut dyn erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_none()
	}

	fn to_string(&self) -> crate::Val {
		crate::Val::new_atomic("nil".to_owned())
	}

	fn is_nil(&self) -> bool {
		true
	}

	fn to_bool(&self) -> crate::Val {
		crate::bool::get(false)
	}
}

impl crate::SameOps for Nil {
	fn cmp(&self, _that: &Self) -> Result<std::cmp::Ordering,crate::Val> {
		Ok(std::cmp::Ordering::Equal)
	}
}
