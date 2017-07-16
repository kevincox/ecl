extern crate gc;

use std::boxed::FnBox;
use std::fmt;
use std::mem;
use std::rc::Rc;

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
	pub fn new<F: FnOnce(Vec<::Val>) -> ::Val + 'static>(refs: Vec<::Val>, code: F) -> ::Val {
		::Val::new(Thunk(gc::GcCell::new(State::Code(refs, Box::new(code)))))
	}
	
	pub fn lazy(p: ::Val, almost: Rc<::Almost>) -> ::Val {
		Self::new(vec![p], move |refs| almost.complete(refs[0].clone()))
	}
	
	fn eval(&self) -> ::Val {
		match *self.0.borrow() {
			State::Val(ref v) => return v.clone(),
			_ => {},
		}
		
		let state = mem::replace(&mut*self.0.borrow_mut(), State::Working);
		let (refs, code) = match state {
			State::Val(_) => unreachable!(),
			State::Working => unreachable!(),
			State::Code(refs, code) => (refs, code),
		};
		let v = code(refs);
		mem::replace(&mut*self.0.borrow_mut(), State::Val(v.clone()));
		v
	}
}

impl ::Value for Thunk {
	fn get(&self) -> Option<::Val> {
		Some(self.eval().clone())
	}
	
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
		match *self.0.borrow() {
			State::Code(_,_) => write!(f, "<code>"),
			State::Working => write!(f, "<evaling>"),
			State::Val(ref v) => write!(f, "Thunk({:?})", v),
		}
	}
}
