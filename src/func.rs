use std::fmt;
use std::rc::Rc;

use dict;

#[derive(Debug)]
pub enum Arg {
	One(String),
	Dict(Vec<(String,bool,::Almost)>),
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
	
	fn call(&self, arg: ::Val) -> ::Val {
		let scope = match self.data.arg {
			Arg::One(ref s) => dict::ADict::new(s.clone(), arg),
			Arg::Dict(ref args) => {
				let val = ::dict::Dict::new(self.parent.clone(), &[]);
				
				{
					let dict = val.downcast_ref::<::dict::Dict>();
					let mut passed_used = 0;
					for &(ref k, required, ref default) in args {
						let passed = arg.index_str(&k);
						if passed != ::Val::new(::nil::Nil) {
							passed_used += 1;
							dict._set_val(k.clone(), ::dict::DictVal::Priv(passed));
						} else if required {
							panic!("Error: required argument {:?} not found.", k);
						} else {
							let default = default.complete(val.clone());
							dict._set_val(k.clone(), ::dict::DictVal::Priv(default));
						}
					}
					
					assert_eq!(passed_used, arg.len());
				}
				
				val
			},
		};
		
		self.data.body.complete(scope)
	}
}

impl ::SameOps for Func { }

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "->{:?} {:?}", self.data.arg, self.data.body)
	}
}
