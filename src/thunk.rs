use gc;
use std;

pub enum State<T: 'static + gc::Trace> {
	Code(T, &'static Fn(T) -> ::Val),
	Working,
	Val(::Val),
}

unsafe impl<T: 'static + gc::Trace> gc::Trace for State<T> {
	custom_trace!(this, {
		match *this {
			State::Code(ref data, _) => mark(data),
			State::Working => {},
			State::Val(ref v) => mark(v),
		}
	});
}

#[derive(Trace)]
pub struct Thunk<T: 'static + gc::Trace>(gc::GcCell<State<T>>);

impl<T: 'static + gc::Trace> Thunk<T> {
	pub fn new(data: T, code: &'static Fn(T) -> ::Val) -> gc::Gc<Thunky> {
		gc::Gc::new(Thunk(gc::GcCell::new(State::Code(data, code))))
	}
	
	pub fn shim(v: ::Val) -> gc::Gc<Thunk<T>> {
		gc::Gc::new(Thunk(gc::GcCell::new(State::Val(v))))
	}
}

pub fn shim(v: ::Val) -> gc::Gc<Thunky> {
	Thunk::<()>::shim(v)
}

pub fn bytecode(pstruct: ::Val, code: ::bytecode::Value) -> gc::Gc<Thunky> {
	const F: &Fn((::Val, ::bytecode::Value)) -> ::Val = &|a| a.1.eval(a.0);
	Thunk::new((pstruct, code), F)
}

pub trait Thunky: gc::Trace + std::fmt::Debug {
	fn eval(&self) -> ::Val;
}

impl<T: 'static + gc::Trace> Thunky for Thunk<T> {
	fn eval(&self) -> ::Val {
		if let State::Val(ref v) = *self.0.borrow_mut() {
			return v.clone()
		}
		
		let state = std::mem::replace(&mut*self.0.borrow_mut(), State::Working);
		let (data, code) = match state {
			State::Val(_) => unreachable!(),
			State::Working =>
				return ::err::Err::new("Dependency cycle detected.".to_owned()),
			State::Code(data, code) => (data, code),
		};
		let v = code(data);
		std::mem::replace(&mut*self.0.borrow_mut(), State::Val(v.clone()));
		v
	}
}

impl<T: 'static + gc::Trace> std::fmt::Debug for Thunk<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match *self.0.borrow_mut() {
			State::Code(_,_) => write!(f, "<code>"),
			State::Working => write!(f, "<evaling>"),
			State::Val(ref v) => write!(f, "Thunk({:?})", v),
		}
	}
}
