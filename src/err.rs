use std;

#[derive(Clone)]
pub struct Err {
	msg: String,
	loc: crate::grammar::Loc,
	chained: crate::Inline,
}

impl Err {
	pub fn new(msg: String) -> crate::Val {
		Self::new_at(crate::grammar::Loc{line: 0, col: 0}, msg)
	}

	pub fn new_at(loc: crate::grammar::Loc, msg: String) -> crate::Val {
		crate::Val::new_atomic(Err{msg, loc, chained: crate::Inline::Nil})
	}

	pub fn new_from_at(chained: crate::Val, loc: crate::grammar::Loc, msg: String) -> crate::Val {
		debug_assert!(chained.is_err(), "Expected to chain to error got {:#?}", chained);
		crate::Val::new(chained.pool.clone(), Err{msg, loc, chained: chained.value})
	}

	fn from(e: &std::error::Error) -> crate::Val {
		if let Some(sub) = e.source() {
			Self::from(sub).annotate(e.description())
		} else {
			Err::new(e.description().to_string())
		}
	}
}

impl crate::Value for Err {
	fn type_str(&self) -> &'static str { "err" }
	fn is_err(&self) -> bool { true }

	fn eval(&self) -> Result<(),crate::Val> {
		Err(crate::Val::new_atomic((*self).clone()))
	}
}

impl crate::SameOps for Err {
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

impl<E: std::error::Error> std::convert::From<E> for crate::Val {
	fn from(e: E) -> Self {
		Err::from(&e)
	}
}
