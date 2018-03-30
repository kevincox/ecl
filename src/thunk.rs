use std;
use std::rc::Rc;

pub enum State<T: 'static> {
	Code(::mem::WeakPoolHandle, T, &'static Fn(T) -> ::Val),
	Working,
	Val(::mem::WeakPoolHandle, ::Inline),
}

pub struct Thunk<T: 'static>(std::cell::RefCell<State<T>>);

impl<T: 'static> Thunk<T> {
	pub fn new(
		pool: ::mem::WeakPoolHandle,
		data: T,
		code: &'static Fn(T) -> ::Val,
	) -> Rc<Thunky> {
		Rc::new(Thunk(std::cell::RefCell::new(State::Code(pool, data, code))))
	}

	pub fn shim(pool: ::mem::WeakPoolHandle, v: ::Inline) -> Rc<Thunk<T>> {
		Rc::new(Thunk(std::cell::RefCell::new(State::Val(pool, v))))
	}
}

pub fn shim(v: ::Val) -> Rc<Thunky> {
	Thunk::<()>::shim(v.pool.downgrade(), v.value)
}

pub fn bytecode(
	pool: ::mem::WeakPoolHandle,
	parent: Rc<::Parent>,
	code: ::bytecode::Value,
) -> Rc<Thunky> {
	const F: &Fn((Rc<::Parent>, ::bytecode::Value)) -> ::Val = &|a| a.1.eval(a.0);
	Thunk::new(pool, (parent, code), F)
}

pub trait Thunky:  std::fmt::Debug {
	fn eval(&self) -> ::Val;
}

impl<T: 'static> Thunky for Thunk<T> {
	fn eval(&self) -> ::Val {
		if let State::Val(ref p, ref v) = *self.0.borrow_mut() {
			return ::Val{
				pool: p.upgrade(),
				value: v.clone(),
			}
		}

		let state = std::mem::replace(&mut*self.0.borrow_mut(), State::Working);
		let (pool, data, code) = match state {
			State::Val(..) => unreachable!(),
			State::Working =>
				return ::err::Err::new("Dependency cycle detected.".to_owned()),
			State::Code(pool, data, code) => (pool, data, code),
		};
		let v = code(data);
		pool.merge(v.pool.clone());
		std::mem::replace(&mut*self.0.borrow_mut(), State::Val(pool, v.value.clone()));
		v
	}
}

impl<T: 'static> std::fmt::Debug for Thunk<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match *self.0.borrow_mut() {
			State::Code(..) => write!(f, "<code>"),
			State::Working => write!(f, "<evaling>"),
			State::Val(_, ref v) => write!(f, "Thunk({:?})", v),
		}
	}
}
