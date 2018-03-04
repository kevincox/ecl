extern crate gc;

use std::boxed::FnBox;
use std::fmt;
use std::mem;

enum State {
	Code(Vec<::Val>, Box<FnBox(Vec<::Val>) -> ::Val>),
	Working,
	Val(::Val),
}

#[derive(Trace)]
pub struct Thunk(gc::GcCell<State>);

unsafe impl gc::Trace for State {
	custom_trace!(this, {
		match *this {
			State::Code(ref refs, _) => mark(refs),
			State::Working => {},
			State::Val(ref v) => mark(v),
		}
	});
}

impl Thunk {
	pub fn new<F: FnOnce(Vec<::Val>) -> ::Val + 'static>(refs: Vec<::Val>, code: F) -> Self {
		Thunk(gc::GcCell::new(State::Code(refs, Box::new(code))))
	}
	
	pub fn bytecode(pstruct: ::Val, code: ::bytecode::Value) -> Self {
		Self::new(vec![pstruct],
			move |mut refs| code.eval(refs.pop().unwrap()))
	}
	
	pub fn shim(v: ::Val) -> Self {
		Thunk(gc::GcCell::new(State::Val(v)))
	}
	
	pub fn stub() -> Self {
		Thunk(gc::GcCell::new(State::Working))
	}
	
	pub fn eval(&self) -> ::Val {
		if let State::Val(ref v) = *self.0.borrow_mut() {
			return v.clone()
		}
		
		let state = mem::replace(&mut*self.0.borrow_mut(), State::Working);
		let (refs, code) = match state {
			State::Val(_) => unreachable!(),
			State::Working =>
				return ::err::Err::new("Dependency cycle detected.".to_owned()),
			State::Code(refs, code) => (refs, code),
		};
		let v = code(refs);
		mem::replace(&mut*self.0.borrow_mut(), State::Val(v.clone()));
		v
	}
}

impl ::Value for Thunk {
	fn type_str(&self) -> &'static str { "thunk" }
}

impl ::SameOps for Thunk { }

impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl fmt::Debug for Thunk {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self.0.borrow_mut() {
			State::Code(_,_) => write!(f, "<code>"),
			State::Working => write!(f, "<evaling>"),
			State::Val(ref v) => write!(f, "Thunk({:?})", v),
		}
	}
}
