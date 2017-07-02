use std::fmt;
use std::rc;

use dict;
use thunk;

#[derive(Clone,Debug)]
pub enum Arg {
	One(String),
	Dict(Vec<(String,bool)>, rc::Rc<Vec<dict::AlmostDictElement>>),
}

#[derive(Trace)]
pub struct Func {
	parent: ::Val,
	#[unsafe_ignore_trace]
	arg: Arg,
	#[unsafe_ignore_trace]
	body: rc::Rc<::Almost>,
}

impl Func {
	pub fn new(p: ::Val, arg: Arg, body: rc::Rc<::Almost>) -> ::Val {
		::Val::new(Func { parent: p, arg: arg, body: body })
	}
}

impl ::Value for Func {
	fn type_str(&self) -> &'static str { "func" }
	
	fn call(&self, arg: ::Val) -> ::Val {
		let body = self.body.clone();
		let scope = match self.arg {
			Arg::One(ref s) => dict::ADict::new(self.parent.clone(), s.clone(), arg),
			Arg::Dict(ref keys, ref members) => {
				let mut dict = ::dict::Dict::new_raw(self.parent.clone(), members.clone());
				
				for &(ref k, required) in keys {
					let passed = arg.index_str(&k);
					if passed != ::Val::new(::nil::Nil) {
						dict._set_val(k.clone(), passed);
					} else if required {
						panic!("Error: required argument {:?} not found.", k);
					}
				}
				
				dict.to_val()
			},
		};
		thunk::Thunk::new(vec![scope, self.parent.clone()], move |r| {
			body.complete(r[0].clone())
		})
	}
}

impl ::SameOps for Func { }

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?} -> <body>", self.arg)
	}
}
