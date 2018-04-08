use erased_serde;
use std;
use std::rc::Rc;

// The next functions allow something string-like to implement SameOpsTrait
// easily. Unfortunately I can't make them into a trait that does so because it
// would "conflict" with ::SameOps.

fn str_add(this: &str, that: &::Value) -> ::Val {
	if let Some(s) = that.get_str() {
		::Val::new_atomic(format!("{}{}", this, s))
	} else {
		::err::Err::new(format!("Can't add {:?} and {:?}", this, that))
	}
}

fn str_cmp(this: &str, that: &::Value) -> Result<std::cmp::Ordering,::Val> {
	if let Some(s) = that.get_str() {
		Ok(this.cmp(s))
	} else {
		Err(::err::Err::new(format!("Can't compare {:?} and {:?}", this, that)))
	}
}

fn str_eq(this: &str, that: &::Value) -> ::Val {
	if let Some(s) = that.get_str() {
		::bool::get(this == s)
	} else {
		::bool::get(false)
	}
}

impl ::Value for String {
	fn type_str(&self) -> &'static str { "string" }

	fn get_str(&self) -> Option<&str> {
		Some(self)
	}

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self)
	}
}

impl ::SameOpsTrait for String {
	fn as_any(&self) -> &std::any::Any { self }

	fn add(&self, that: &::Value) -> ::Val { str_add(self, that) }
	fn cmp(&self, that: &::Value) -> Result<std::cmp::Ordering,::Val> { str_cmp(self, that) }
	fn eq(&self, that: &::Value) -> ::Val { str_eq(self, that) }
}

pub struct CodeString {
	pub module: Rc<::bytecode::Module>,
	pub offset: usize,
	pub len: usize,
}

impl CodeString {
	fn get(&self) -> &str {
		let bytes = &self.module.code[self.offset..][..self.len];
		unsafe { std::str::from_utf8_unchecked(bytes) }
	}
}

impl ::Value for CodeString {
	fn type_str(&self) -> &'static str { "string" }

	fn get_str(&self) -> Option<&str> {
		Some(self.get())
	}

	fn serialize(&self,  _: &mut Vec<*const ::Value>, s: &mut erased_serde::Serializer)
		-> Result<(),erased_serde::Error> {
		s.erased_serialize_str(self.get())
	}
}

impl ::SameOpsTrait for CodeString {
	fn as_any(&self) -> &std::any::Any { self }

	fn add(&self, that: &::Value) -> ::Val { str_add(self.get(), that) }
	fn cmp(&self, that: &::Value) -> Result<std::cmp::Ordering,::Val> { str_cmp(self.get(), that) }
	fn eq(&self, that: &::Value) -> ::Val { str_eq(self.get(), that) }
}

impl std::fmt::Debug for CodeString {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		self.get().fmt(f)
	}
}
