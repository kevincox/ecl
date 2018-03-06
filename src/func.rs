use std;

#[derive(PartialEq)]
pub enum Arg {
	One(String),
	Dict(Vec<(String,bool,::Almost)>),
	List(Vec<(String,bool,::Almost)>),
}

impl std::fmt::Debug for Arg {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
	pstruct: ::Val,
	body: ::bytecode::Func,
}

#[derive(Debug,PartialEq)]
pub struct FuncData {
	pub arg: Arg,
	pub body: ::Almost,
}

impl Func {
	pub fn new(parent: ::Val, body: ::bytecode::Func) -> ::Val {
		::Val::new(Func{pstruct: parent, body})
	}
}

impl ::Value for Func {
	fn type_str(&self) -> &'static str { "func" }
	
	fn call(&self, arg: ::Val) -> ::Val {
		self.body.call(self.pstruct.clone(), arg)
	}
}

impl ::SameOps for Func { }

impl std::fmt::Debug for Func {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Func({:?})", self.body)
	}
}
