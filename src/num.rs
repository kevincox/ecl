use std;
use erased_serde;

impl crate::Value for f64 {
	fn type_str(&self) -> &'static str { "num" }

	fn serialize(&self,  _: &mut Vec<*const crate::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_f64(*self)
	}

	fn to_string(&self) -> crate::Val {
		crate::Val::new_atomic(ToString::to_string(self))
	}

	fn get_num(&self) -> Option<f64> {
		Some(*self)
	}

	fn neg(&self) -> crate::Val {
		crate::Val::new_atomic(-self)
	}
}

impl crate::SameOps for f64 {
	fn add(&self, that: &Self) -> crate::Val {
		crate::Val::new_num(*self + *that)
	}

	fn subtract(&self, that: &Self) -> crate::Val {
		crate::Val::new_num(*self - *that)
	}

	fn cmp(&self, that: &Self) -> Result<std::cmp::Ordering,crate::Val> {
		self.partial_cmp(that)
			.ok_or_else(||
				crate::err::Err::new(format!("Failed to compare {:?} and {:?}", *self, *that)))
	}
}
