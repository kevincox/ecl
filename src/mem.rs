use std;
use std::rc::Rc;

#[derive(Debug)]
pub struct Pool {
	strong: std::cell::Cell<isize>,
	weak: std::cell::Cell<isize>,
	state: std::cell::RefCell<State>,
}

impl Pool {
	fn new() -> Handle {
		Handle::new(Box::into_raw(Box::new(Pool{
			strong: std::cell::Cell::new(1),
			weak: std::cell::Cell::new(0),
			state: std::cell::RefCell::new(State::Owning(Data{
				allocated: Vec::new(),
			})),
		})))
	}
}

#[derive(Debug)]
enum State {
	Merged(Handle),
	Owning(Data),
	Dead,
}

impl Drop for State {
	fn drop(&mut self) {
		// eprintln!("Drop {:?}", self);
	}
}

#[derive(Debug)]
pub struct Data {
	allocated: Vec<Rc<crate::Value>>,
}

#[derive(Clone,Debug)]
struct Handle(std::cell::Cell<*mut Pool>);

impl Handle {
	fn new(p: *mut Pool) -> Self {
		Handle(std::cell::Cell::new(p))
	}

	fn is_empty(&self) -> bool {
		self.0.get().is_null()
	}

	fn get(&self) -> &Pool {
		let this = unsafe { &*self.0.get() };
		match *this.state.borrow_mut() {
			State::Merged(ref handle) => crate::i_promise_this_will_stay_alive(handle.get()),
			State::Owning(_) => crate::i_promise_this_will_stay_alive(this),
			State::Dead => panic!("Dereferencing dead weak handle."),
		}
	}

	fn alive(&self) -> bool {
		let this = unsafe { &*self.0.get() };

		if this.strong.get() > 0 { return true }
		debug_assert!(this.weak.get() > 0,
			"Calling method on Weak{{strong: {}, weak: {}}}",
			this.strong.get(),
			this.weak.get());

		match *this.state.borrow_mut() {
			State::Merged(ref handle) => handle.alive(),
			State::Owning(_) => false,
			State::Dead => false,
		}
	}

	fn inc_strong(&self) {
		debug_assert!(self.alive());
		let this = unsafe { &*self.0.get() };
		let strong = this.strong.get();
		this.strong.set(strong + 1);
		if strong == 0 {
			if let State::Merged(ref handle) = *this.state.borrow_mut() {
				handle.inc_strong()
			}
		}
	}

	fn dec_strong(&self) {
		let this = unsafe { &*self.0.get() };
		let strong = this.strong.get();
		let weak = this.weak.get();
		debug_assert_ne!(strong, 0);
		this.strong.set(strong - 1);
		if strong == 1 {
			let next = if let State::Merged(ref handle) = *this.state.borrow_mut() {
				handle.0.get()
			} else {
				std::ptr::null_mut()
			};
			if next != std::ptr::null_mut() {
				Handle::new(next).dec_strong();
			}

			if weak == 0 {
				unsafe { Box::from_raw(self.0.get()); };
				return
			}

			if next != std::ptr::null_mut() {
				return
			}

			let old = std::mem::replace(&mut *this.state.borrow_mut(), State::Dead);
			if let State::Dead = old { unreachable!() }
			std::mem::drop(old);
		}
	}

	fn inc_weak(&self) {
		debug_assert!(self.alive());
		let this = unsafe { &*self.0.get() };
		let weak = this.weak.get();
		this.weak.set(weak + 1);
		if weak == 0 {
			if let State::Merged(ref handle) = *this.state.borrow_mut() {
				handle.inc_weak()
			}
		}
	}

	fn dec_weak(&self) {
		let this = unsafe { &*self.0.get() };
		let strong = this.strong.get();
		let weak = this.weak.get();
		debug_assert_ne!(weak, 0);
		this.weak.set(weak - 1);
		if weak == 1 {
			if let State::Merged(ref handle) = *this.state.borrow_mut() {
				handle.dec_weak();
			}

			if strong == 0 {
				unsafe { Box::from_raw(self.0.get()); };
			}
		}
	}

	pub fn merge(&self, pool: Handle, this_strong: bool) {
		let this_pool = self.get();
		let handle = Handle::new(this_pool as *const Pool as *mut Pool);
		if this_pool as *const Pool != self.0.get() as *const Pool {
			if this_strong { handle.inc_strong() } else { handle.inc_weak() }
			if this_strong { self.dec_strong() } else { self.dec_weak() }
			self.0.set(handle.0.get());
		}
		let that_pool = pool.get();
		if that_pool as *const Pool == self.0.get() as *const Pool {
			return
		}

		pool.0.set(handle.0.get());

		if that_pool.strong.get() > 0 { handle.inc_strong(); }
		if that_pool.weak.get() > 0 { handle.inc_weak(); }

		let mut this_state = this_pool.state.borrow_mut();
		let mut that_state = that_pool.state.borrow_mut();

		// eprintln!("merge(\n\t{:?}\n\t{:?})", this_state, that_state);
		match (&mut *this_state, &mut *that_state) {
			(&mut State::Owning(ref mut this_data), &mut State::Owning(ref mut that_data)) => {
				this_data.allocated.extend(that_data.allocated.drain(..));
			}
			other => unreachable!("Expected two owned: {:?}", other),
		}
		*that_state = State::Merged(handle);
	}
}

#[derive(Debug)]
pub struct PoolHandle(Handle);

impl PoolHandle {
	pub fn new() -> Self {
		let ptr = Pool::new();
		PoolHandle(ptr)
	}

	pub fn none() -> Self {
		PoolHandle(Handle(std::cell::Cell::new(std::ptr::null_mut())))
	}

	fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	pub fn downgrade(&self) -> WeakPoolHandle {
		debug_assert!(!self.is_empty());

		self.0.inc_weak();
		WeakPoolHandle(self.0.clone())
	}

	pub fn push(&self, v: Rc<crate::Value>) {
		debug_assert!(!self.is_empty());

		let pool = self.0.get();
		// eprintln!("push({:?})", v);
		match *pool.state.borrow_mut() {
			State::Owning(ref mut data) => data.allocated.push(v),
			ref other => unreachable!("Pool state: {:?}", other),
		}
	}

	pub fn merge(&self, h: PoolHandle) {
		debug_assert!(!self.is_empty());
		if h.is_empty() { return }
		self.0.merge(h.0.clone(), true)
	}
}

impl Clone for PoolHandle {
	fn clone(&self) -> Self {
		if self.is_empty() { return PoolHandle(self.0.clone()) }

		self.0.inc_strong();
		PoolHandle(self.0.clone())
	}
}

impl Drop for PoolHandle {
	fn drop(&mut self) {
		if self.is_empty() { return }
		self.0.dec_strong()
	}
}

#[derive(Debug)]
pub struct WeakPoolHandle(Handle);

impl WeakPoolHandle {
	pub fn merge(&self, h: PoolHandle) {
		if h.is_empty() { return }
		self.0.merge(h.0.clone(), false)
	}

	pub fn upgrade(&self) -> PoolHandle {
		assert!(self.0.alive(), "Upgrading dead WeakPoolHandle");
		self.0.inc_strong();
		PoolHandle(self.0.clone())
	}
}

impl Clone for WeakPoolHandle {
	fn clone(&self) -> Self {
		self.0.inc_weak();
		WeakPoolHandle(self.0.clone())
	}
}

impl Drop for WeakPoolHandle {
	fn drop(&mut self) {
		self.0.dec_weak();
	}
}
