extern crate gc;

use std::fmt;
use std::cell;

// #[derive(Trace)]
pub struct Thunk {
	// #[unsafe_ignore_trace]
	code: Box<Fn(&Vec<::Val>) -> ::Val>,
	refs: Vec<::Val>,
	data: cell::RefCell<Option<::Val>>,
}

unsafe impl gc::Trace for Thunk {
	custom_trace!(this, {
		mark(&this.refs);
		#[allow(unused_unsafe)]
		unsafe { mark(&*this.data.as_ptr()); }
	});
}

impl Thunk {
	pub fn new<F: Fn(&Vec<::Val>) -> ::Val + 'static>(refs: Vec<::Val>, code: F) -> ::Val {
		::Val::new(Thunk{
			code: Box::new(code),
			refs: refs,
			data: cell::RefCell::new(None)
		})
	}
	
	fn eval(&self) -> ::Val {
		let mut data = self.data.borrow_mut();
		if data.is_none() {
			let new = (self.code)(&self.refs);
			unsafe { gc::Trace::unroot(&new.0) };
			*data = Some(new);
		}
		data.as_ref().unwrap().clone()
	}
}

impl ::Valu for Thunk {
	fn get(&self) -> Option<::Val> {
		Some(self.eval().clone())
	}
	
	fn type_str(&self) -> &'static str { "thunk" }
}

impl ::ValuAdd for Thunk { }

impl PartialEq for Thunk {
	fn eq(&self, _that: &Self) -> bool {
		false
	}
}

impl fmt::Debug for Thunk {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self.data.try_borrow() {
			Ok(data) => match *data {
				Some(ref v) => v.fmt(f),
				None => write!(f, "<code>"),
			},
			Err(_) => write!(f, "<evaling>"),
		}
	}
}
