use std;
use std::rc::Rc;

#[derive(PartialEq)]
pub enum Arg {
	One(String),
	Dict(Vec<(String,bool,crate::Almost)>),
	List(Vec<(String,bool,crate::Almost)>),
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

pub struct Func {
	parent: Rc<crate::Parent>,
	body: crate::bytecode::Func,
}

#[derive(Debug,PartialEq)]
pub struct FuncData {
	pub arg: Arg,
	pub body: crate::Almost,
}

impl Func {
	pub fn new(parent: Rc<crate::Parent>, body: crate::bytecode::Func) -> crate::Val {
		crate::Val::new_atomic(Func{parent, body})
	}
}

impl crate::Value for Func {
	fn type_str(&self) -> &'static str { "func" }

	fn call(&self, arg: crate::Val) -> crate::Val {
		self.body.call(self.parent.clone(), arg)
	}
}

impl crate::SameOps for Func { }

impl std::fmt::Debug for Func {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Func({:?})", self.body)
	}
}
