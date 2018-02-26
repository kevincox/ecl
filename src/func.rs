use std::fmt;
use std::rc::Rc;

use dict;

#[derive(PartialEq)]
pub enum Arg {
	One(String),
	Dict(Vec<(String,bool,::Almost)>),
	List(Vec<(String,bool,::Almost)>),
}

impl fmt::Debug for Arg {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Arg::One(ref s) => write!(f, "{}", s),
			Arg::Dict(ref args) => {
				write!(f, "{{")?;
				let mut first = true;
				for arg in args {
					if first { first = false } else { write!(f, " ")? }
					write!(f, "{}", arg.0)?;
					if arg.1 { write!(f, "={:?}", arg.2)?  }
				}
				write!(f, "}}")
			}
			Arg::List(ref args) => {
				write!(f, "[")?;
				let mut first = true;
				for arg in args {
					if first { first = false } else { write!(f, " ")? }
					write!(f, "{}", arg.0)?;
					if arg.1 { write!(f, "={:?}", arg.2)?  }
				}
				write!(f, "]")
			}
		}
	}
}

#[derive(Trace)]
pub struct Func {
	plex: ::Val,
	pstruct: ::Val,
	#[unsafe_ignore_trace]
	data: Rc<FuncData>,
}

#[derive(Debug,PartialEq)]
pub struct FuncData {
	pub arg: Arg,
	pub body: ::Almost,
}

impl Func {
	pub fn new(plex: ::Val, pstruct: ::Val, data: Rc<FuncData>) -> ::Val {
		::Val::new(Func {plex, pstruct, data})
	}
}

impl ::Value for Func {
	fn type_str(&self) -> &'static str { "func" }
	
	fn call(&self, arg: ::Val) -> ::Val {
		let scope = match self.data.arg {
			Arg::One(ref s) => {
				let scope = dict::Dict::new(self.plex.clone(), self.pstruct.clone(), &[]);
				scope.downcast_ref::<::dict::Dict>().unwrap()
					._set_val(s.clone(), ::dict::DictVal::Prv(arg));
				scope
			},
			Arg::Dict(ref args) => {
				let scope = ::dict::Dict::new(self.plex.clone(), self.pstruct.clone(), &[]);
				let pstruct = ::Val::new(::dict::ParentSplitter{
					parent: scope.clone(),
					grandparent: self.pstruct.clone(),
				});
				{
					let dict = scope.downcast_ref::<::dict::Dict>().unwrap();
					let mut passed_used = 0;
					for &(ref k, required, ref default) in args {
						let passed = arg.index_str(&k);
						if passed != ::nil::get() {
							passed_used += 1;
							dict._set_val(k.clone(), ::dict::DictVal::Prv(passed));
						} else if required {
							panic!("Error: required argument {:?} not found.", k);
						} else {
							let default = default.complete(scope.clone(), pstruct.clone());
							dict._set_val(k.clone(), ::dict::DictVal::Prv(default));
						}
					}
					
					assert_eq!(passed_used, arg.len());
				}
				
				scope
			},
			Arg::List(ref args) => {
				let scope = ::dict::Dict::new(self.plex.clone(), ::nil::get(), &[]);
				let pstruct = ::Val::new(::dict::ParentSplitter{
					parent: scope.clone(),
					grandparent: self.pstruct.clone(),
				});
				
				{
					let dict = scope.downcast_ref::<::dict::Dict>().unwrap();
					
					let arg_len = arg.len();
					if arg_len > args.len() {
						return ::err::Err::new(
							format!(
								"Too many elements for destructure. Got {}, \
								expected at most {}",
								arg_len, args.len()));
					}
					let mut arg_iter = match arg.iter() {
						Some(iter) => iter,
						None => return ::err::Err::new(
							format!("Can't destructure {:?} as a list", arg)),
					};
					for &(ref k, required, ref default) in args {
						if let Some(val) = arg_iter.next() {
							dict._set_val(k.clone(), ::dict::DictVal::Prv(val));
						} else if required {
							return ::err::Err::new(
								format!(
									"Not enough elements in list to \
									destructure {:?}.",
									k));
						} else {
							let default = default.complete(scope.clone(), pstruct.clone());
							dict._set_val(k.clone(), ::dict::DictVal::Prv(default));
						}
					}
				}
				
				scope
			},
		};
		let pstruct = ::Val::new(::dict::ParentSplitter{
			parent: scope.clone(),
			grandparent: self.pstruct.clone(),
		});
		
		self.data.body.complete(scope, pstruct)
	}
}

impl ::SameOps for Func { }

impl fmt::Debug for Func {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "->{:?} {:?}", self.data.arg, self.data.body)
	}
}
