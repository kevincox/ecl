use erased_serde;
use std::rc::Rc;

#[derive(Debug)]
pub struct Nil;

thread_local! {
	static NIL: Rc<Nil> = Rc::new(Nil);
}

pub fn get() -> ::Val {
	::Val{
		pool: ::mem::PoolHandle::new(),
		value: NIL.with(|n| Rc::downgrade(n)),
	}
}
// pub fn get() -> ::Val { ::Val::new_atomic(Nil) }

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
		::bool::get_true()
	}
}
