use erased_serde;
use std::rc::Rc;

thread_local! {
	static TRUE: Rc<bool> = Rc::new(true);
	static FALSE: Rc<bool> = Rc::new(false);
}

pub fn get(b: bool) -> ::Val { if b { get_true() } else { get_false() } }
pub fn get_true() -> ::Val {
	::Val{
		pool: ::mem::PoolHandle::new(),
		value: TRUE.with(|n| Rc::downgrade(n)),
	}
}
pub fn get_false() -> ::Val {
	::Val{
		pool: ::mem::PoolHandle::new(),
		value: FALSE.with(|n| Rc::downgrade(n)),
	}
}

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
