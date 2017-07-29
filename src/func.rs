use std::fmt;
use std::rc::Rc;

use dict;

pub enum Arg {
	One(String),
	Dict(Vec<(String,bool,::Almost)>),
}

impl fmt::Debug for Arg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Arg::One(ref s) => write!(f, "{}", s),
			Arg::Dict(ref args) => {
				write!(f, "{{")?;
				let mut first = true;
				for arg in args {
					if first { first = false } else { write!(f, ", ")? }
					write!(f, "{}", arg.0)?;
					if arg.1 { write!(f, "={:?}", arg.2)?  }
				}
				write!(f, "}}")
			}
		}
	}
}

#[derive(Trace)]
pub struct Func {
	parent: ::Val,
	#[unsafe_ignore_trace]
	data: Rc<FuncData>,
}

pub struct FuncData {
	pub arg: Arg,
	pub body: ::Almost,
}

impl Func {
	pub fn new(p: ::Val, data: Rc<FuncData>) -> ::Val {
		::Val::new(Func { parent: p, data: data })
	}
}

impl ::Value for Func {
	fn type_str(&self) -> &'static str { "func" }
	
	fn call(&self, _this: ::Val, arg: ::Val) -> ::Val {
		let scope = match self.data.arg {
			Arg::One(ref s) => dict::ADict::new(self.parent.clone(), s.clone(), arg),
			Arg::Dict(ref args) => {
				let val = ::dict::Dict::new(self.parent.clone(), ::nil::get(), &[]);
				
				{
					let dict = val.downcast_ref::<::dict::Dict>();
					let mut passed_used = 0;
					for &(ref k, required, ref default) in args {
						let passed = arg.index_str(&k);
						if passed != ::nil::get() {
							passed_used += 1;
							dict.unwrap()._set_val(k.clone(), ::dict::DictVal::private(passed));
						} else if required {
							panic!("Error: required argument {:?} not found.", k);
						} else {
							let default = default.complete(val.clone(), ::nil::get());
							dict.unwrap()._set_val(k.clone(), ::dict::DictVal::private(default));
						}
					}
					
					assert_eq!(passed_used, arg.len());
				}
				
				val
			},
		};
		
		self.data.body.complete(scope, ::nil::get())
	}
}

impl ::SameOps for Func { }

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "->{:?} {:?}", self.data.arg, self.data.body)
	}
}
