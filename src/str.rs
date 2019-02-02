use erased_serde;
use std;
use std::rc::Rc;

// The next functions allow something string-like to implement SameOpsTrait
// easily. Unfortunately I can't make them into a trait that does so because it
// would "conflict" with ::SameOps.

fn str_add(this: &str, that: &crate::Value) -> crate::Val {
	if let Some(s) = that.get_str() {
		crate::Val::new_atomic(format!("{}{}", this, s))
	} else {
		crate::err::Err::new(format!("Can't add {:?} and {:?}", this, that))
	}
}

fn str_cmp(this: &str, that: &crate::Value) -> Result<std::cmp::Ordering,crate::Val> {
	if let Some(s) = that.get_str() {
		Ok(this.cmp(s))
	} else {
		Err(crate::err::Err::new(format!("Can't compare {:?} and {:?}", this, that)))
	}
}

impl crate::Value for String {
	fn type_str(&self) -> &'static str { "string" }

	fn get_str(&self) -> Option<&str> {
		Some(self)
	}

	fn serialize(&self,  _: &mut Vec<*const crate::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self)
	}
}

impl crate::SameOpsTrait for String {
	fn as_any(&self) -> &std::any::Any { self }

	fn add(&self, that: &crate::Value) -> crate::Val { str_add(self, that) }
	fn cmp(&self, that: &crate::Value) -> Result<std::cmp::Ordering,crate::Val> { str_cmp(self, that) }
}

pub struct CodeString {
	pub module: Rc<crate::bytecode::Module>,
	pub offset: usize,
	pub len: usize,
}

impl CodeString {
	fn get(&self) -> &str {
		let bytes = &self.module.code[self.offset..][..self.len];
		unsafe { std::str::from_utf8_unchecked(bytes) }
	}
}

impl crate::Value for CodeString {
	fn type_str(&self) -> &'static str { "string" }

	fn get_str(&self) -> Option<&str> {
		Some(self.get())
	}

	fn serialize(&self,  _: &mut Vec<*const crate::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self.get())
	}
}

impl crate::SameOpsTrait for CodeString {
	fn as_any(&self) -> &std::any::Any { self }

	fn add(&self, that: &crate::Value) -> crate::Val { str_add(self.get(), that) }
	fn cmp(&self, that: &crate::Value) -> Result<std::cmp::Ordering,crate::Val> { str_cmp(self.get(), that) }
}

impl std::fmt::Debug for CodeString {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.get().fmt(f)
	}
}
