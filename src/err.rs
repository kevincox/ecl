extern crate serde_yaml;

use std;

#[derive(Clone,Trace)]
pub struct Err {
	msg: String,
	#[unsafe_ignore_trace]
	loc: ::grammar::Loc,
	chained: ::Val
}

impl Err {
	pub fn new(msg: String) -> ::Val {
		Self::new_at(::grammar::Loc{line: 0, col: 0}, msg)
	}
	
	pub fn new_at(loc: ::grammar::Loc, msg: String) -> ::Val {
		Self::new_from_at(::nil::get(), loc, msg)
	}
	
	pub fn new_from_at(chained: ::Val, loc: ::grammar::Loc, msg: String) -> ::Val {
		::Val::new(Err{msg: msg, loc: loc, chained: chained})
	}
	
	fn from(e: &std::error::Error) -> ::Val {
		if let Some(sub) = e.cause() {
			Self::from(sub).annotate(e.description())
		} else {
			Err::new(e.description().to_string())
		}
	}
}

impl ::Value for Err {
	fn type_str(&self) -> &'static str { "err" }
	fn is_err(&self) -> bool { true }
	
	fn eval(&self) -> Result<(),::Val> {
		Err(::Val::new((*self).clone()))
	}
}

impl ::SameOps for Err {
}

impl ::fmt::Debug for Err {
	fn fmt(&self, f: &mut ::fmt::Formatter) -> ::fmt::Result {
		if self.loc.col != 0 {
			write!(f, "At line {}.{} ", self.loc.line, self.loc.col)?;
		}
		writeln!(f, "{}", self.msg)?;
		if self.chained.is_err() {
			write!(f, "{:?}", self.chained)?;
		}
		Ok(())
	}
}

impl<E: std::error::Error> std::convert::From<E> for ::Val {
	fn from(e: E) -> Self {
		Err::from(&e)
	}
}
