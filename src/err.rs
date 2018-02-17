#[derive(Trace)]
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
		Self::new_from(::nil::get(), loc, msg)
	}
	
	pub fn new_from(chained: ::Val, loc: ::grammar::Loc, msg: String) -> ::Val {
		::Val::new(Err{msg: msg, loc: loc, chained: chained})
	}
}

impl ::Value for Err {
	fn type_str(&self) -> &'static str { "err" }
	fn is_err(&self) -> bool { true }
	
	fn structural_lookup(&self, _: usize, _: &::dict::Key) -> Option<::Val> {
		unreachable!("Can't perform structural lookup in error: {:?}", self);
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
