use std;

#[derive(Clone)]
pub struct Err {
	msg: String,
	loc: ::grammar::Loc,
	chained: ::Inline,
}

impl Err {
	pub fn new(msg: String) -> ::Val {
		Self::new_at(::grammar::Loc{line: 0, col: 0}, msg)
	}

	pub fn new_at(loc: ::grammar::Loc, msg: String) -> ::Val {
		::Val::new_atomic(Err{msg, loc, chained: ::Inline::Nil})
	}

	pub fn new_from_at(chained: ::Val, loc: ::grammar::Loc, msg: String) -> ::Val {
		debug_assert!(chained.is_err(), "Expected to chain to error got {:#?}", chained);
		::Val::new(chained.pool.clone(), Err{msg, loc, chained: chained.value})
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
		Err(::Val::new_atomic((*self).clone()))
	}
}

impl ::SameOps for Err {
}

impl std::fmt::Debug for Err {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		if self.loc.col != 0 {
			write!(f, "At line {}.{} ", self.loc.line, self.loc.col)?;
		}
		writeln!(f, "{}", self.msg)?;
		if self.chained.is_err() {
			write!(f, "{:?}", &*self.chained)?;
		}
		Ok(())
	}
}

impl<E: std::error::Error> std::convert::From<E> for ::Val {
	fn from(e: E) -> Self {
		Err::from(&e)
	}
}
