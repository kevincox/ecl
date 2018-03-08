use erased_serde;

thread_local! {
	static TRUE: ::Val = ::Val::new(true);
	static FALSE: ::Val = ::Val::new(false);
}

pub fn get(b: bool) -> ::Val { if b { get_true() } else { get_false() } }
pub fn get_true() -> ::Val { TRUE.with(|t| t.clone()) }
pub fn get_false() -> ::Val { FALSE.with(|f| f.clone()) }

impl ::Value for bool {
	fn type_str(&self) -> &'static str { "bool" }

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_bool(*self)
	}

	fn to_string(&self) -> ::Val {
		::Val::new(ToString::to_string(self))
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
