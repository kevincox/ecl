extern crate erased_serde;

use builtins;

#[derive(Debug,Trace)]
pub struct Nil;

thread_local! {
	static NIL: ::Val = ::Val::new(Nil);
}

pub fn get() -> ::Val { NIL.with(|n| n.clone()) }

impl ::Value for Nil {
	fn type_str(&self) -> &'static str { "nil" }
	
	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_none()
	}
	
	fn to_string(&self) -> String {
		"nil".to_owned()
	}
	
	fn to_bool(&self) -> bool {
		false
	}
	
	fn lookup(&self, key: &str) -> ::Val {
		builtins::get(key)
	}
}

impl ::SameOps for Nil {
	fn eq(&self, _: &Self) -> bool {
		true
	}
}
