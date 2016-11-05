extern crate erased_serde;

use builtins;

#[derive(Debug,Trace)]
pub struct Nil;

impl ::Valu for Nil {
	fn type_str(&self) -> &'static str { "nil" }
	
	fn serialize(&self,  _: &mut Vec<*const ::Valu>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_none()
	}
	
	fn to_string(&self) -> String {
		"nil".to_owned()
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
