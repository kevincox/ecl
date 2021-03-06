use std;
use std::rc::Rc;

pub enum State<T: 'static> {
	Code(T, &'static dyn Fn(T) -> crate::Val),
	Working,
	Val(crate::Inline),
}

pub struct Thunk<T: 'static>(std::cell::RefCell<State<T>>);

impl<T: 'static> Thunk<T> {
	pub fn new(
		data: T,
		code: &'static dyn Fn(T) -> crate::Val,
	) -> Rc<dyn Thunky> {
		Rc::new(Thunk(std::cell::RefCell::new(State::Code(data, code))))
	}

	pub fn shim(v: crate::Inline) -> Rc<Thunk<T>> {
		Rc::new(Thunk(std::cell::RefCell::new(State::Val(v))))
	}
}

pub fn shim(v: crate::Val) -> Rc<dyn Thunky> {
	Thunk::<()>::shim(v.value)
}

pub fn bytecode(parent: Rc<crate::Parent>, code: crate::bytecode::Value) -> Rc<dyn Thunky> {
	const F: &dyn Fn((Rc<crate::Parent>, crate::bytecode::Value)) -> crate::Val =
		&|(parent, val)| val.eval(parent);

	Thunk::new((parent, code), F)
}

pub trait Thunky:  std::fmt::Debug {
	fn eval(&self, pool: crate::mem::PoolHandle) -> crate::Val;
}

impl<T: 'static> Thunky for Thunk<T> {
	fn eval(&self, pool: crate::mem::PoolHandle) -> crate::Val {
		if let State::Val(ref v) = *self.0.borrow_mut() {
			return crate::Val{pool, value: v.clone()}
		}

		let state = std::mem::replace(&mut*self.0.borrow_mut(), State::Working);
		let (data, code) = match state {
			State::Val(..) => unreachable!(),
			State::Working =>
				return crate::err::Err::new("Dependency cycle detected.".to_owned()),
			State::Code(data, code) => (data, code),
		};
		let v = code(data);
		pool.merge(v.pool.clone());
		std::mem::replace(&mut*self.0.borrow_mut(), State::Val(v.value.clone()));
		v
	}
}

impl<T: 'static> std::fmt::Debug for Thunk<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match *self.0.borrow_mut() {
			State::Code(..) => write!(f, "<code>"),
			State::Working => write!(f, "<evaling>"),
			State::Val(ref v) => write!(f, "Thunk({:?})", v),
		}
	}
}
