use std::fmt;
use std::rc;

use dict;
use thunk;

#[derive(Trace)]
pub struct Func {
	parent: ::Val,
	arg: String,
	#[unsafe_ignore_trace]
	body: rc::Rc<::Almost>,
}

impl Func {
	pub fn new(p: ::Val, arg: String, body: rc::Rc<::Almost>) -> ::Val {
		::Val::new(Func { parent: p, arg: arg, body: body })
	}
}

impl ::Valu for Func {
	fn type_str(&self) -> &'static str { "func" }
	
	fn call(&self, arg: ::Val) -> ::Val {
		let body = self.body.clone();
		let scope = dict::ADict::new(self.parent.clone(), self.arg.clone(), arg);
		thunk::Thunk::new(vec![scope, self.parent.clone()], move |r| {
			body.complete(r[0].clone())
		})
	}
}

impl ::SameOps for Func { }

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{} -> <body>", self.arg)
	}
}
