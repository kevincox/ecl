use std;

static LEAK_COUNT: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

struct Leakable;

impl Leakable {
	fn new() -> Self {
		let previous = LEAK_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
		assert_ne!(previous, u32::max_value());
		Leakable
	}
}

impl Drop for Leakable {
	fn drop(&mut self) {
		let previous = LEAK_COUNT.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
		assert_ne!(previous, u32::min_value());
	}
}

static BUILTINS: &[(&str, &(dyn Fn() -> crate::Val + Sync))] = &[
	("nil", &|| crate::nil::get()),
	("cond", &|| new("if", |v| cond(v))),
	("error", &|| new("error", |msg|
		crate::err::Err::new(format!("Error: {:?}", msg)))),
	("index", &|| new("index", |l| Builtin::new("index curried", &[l], |l, i| l[0].index(i)))),
	("false", &|| crate::bool::get(false)),
	("foldl", &|| new("foldl",
		|f| Builtin::new("foldl:func", &[f],
			|d, accum| Builtin::new("foldl:func:accum", &[d[0].clone(), accum.clone()],
				|d, o| o.foldl(d[0].clone(), d[1].clone()))))),
	("foldr", &|| new("foldr",
		|f| Builtin::new("foldr:func", &[f],
			|d, accum| Builtin::new("foldr:func:accum", &[d[0].clone(), accum.clone()],
				|d, o| o.foldr(d[0].clone(), d[1].clone()))))),
	("load", &|| new("load", |path| {
		if path.is_err() { return path }
		match path.get_str() {
			Ok(s) => crate::eval_file(s),
			Err(e) => crate::err::Err::new_from_at(e,
				crate::grammar::Loc{line:0, col: 0},
				format!("load expects string argument, got {:?}", path)),
		}
	})),
	("map", &|| new("map", |f| Builtin::new("map:func", &[f], |f, o| o.map(f[0].clone())))),
	("reverse", &|| new("reverse", |v| v.reverse())),
	("panic", &|| new("panic", |msg|
		panic!("Script called panic: {:?}", msg))),
	("true", &|| crate::bool::get(true)),
	("type", &|| new("type", |v| crate::Val::new_atomic(v.type_str().to_owned()))),
	("_testing_assert_cache_eval", &|| {
		let unevaluated = std::cell::Cell::new(true);
		let func = move |r| {
			assert!(unevaluated.get(), "Called twice");
			unevaluated.set(false);
			r
		};
		new("_testing_assert_cache_eval", func)
	}),
	("_testing_leak_count", &|| {
		crate::Val::new_atomic(LEAK_COUNT.load(std::sync::atomic::Ordering::SeqCst) as f64)
	}),
	("_testing_leak_trace", &|| {
		let tracker = Leakable::new();
		let func = move |r| {
			let _ = tracker;
			r
		};
		new("_testing_leak", func)
	}),
];

pub fn builtin_id(key: &str) -> Option<usize> {
	BUILTINS.iter().position(|p| p.0 == key)
}

pub fn get_id(id: usize) -> crate::Val {
	BUILTINS[id].1()
}

pub struct Builtin<F>{
	name: &'static str,
	func: F,
	pool: crate::mem::WeakPoolHandle,
	data: Vec<crate::Inline>,
}

fn new<F: Fn(crate::Val) -> crate::Val + 'static>(name: &'static str, func: F) -> crate::Val {
	Builtin::new(name, &[], move |_, a| func(a))
}

impl<F: Fn(Vec<crate::Val>, crate::Val) -> crate::Val + 'static> Builtin<F> {
	fn new(name: &'static str, d: &[crate::Val], func: F) -> crate::Val {
		let pool = {
			let mut iter = d.into_iter();
			match iter.next() {
				None => crate::mem::PoolHandle::new(),
				Some(v) => {
					let pool = v.pool.clone();
					for v in iter {
						pool.merge(v.pool.clone());
					}
					pool
				}
			}
		};
		let data = d.into_iter().map(|v| v.value.clone()).collect();
		crate::Val::new(pool.clone(), Builtin{name, func, pool: pool.downgrade(), data})
	}
}

impl<F: Fn(Vec<crate::Val>, crate::Val) -> crate::Val + 'static> crate::SameOps for Builtin<F> {
	fn eq(&self, that: &Self) -> crate::Val {
		crate::bool::get(self as *const Builtin<F> == that as *const Builtin<F>)
	}
}

impl<F: Fn(Vec<crate::Val>, crate::Val) -> crate::Val + 'static> crate::Value for Builtin<F> {
	fn call(&self, arg: crate::Val) -> crate::Val {
		let data = self.data.iter()
			.map(|v| crate::Val{pool: self.pool.upgrade(), value: v.clone()})
			.collect();
		(self.func)(data, arg)
	}

	fn type_str(&self) -> &'static str { "builtin" }
}

impl<F: Fn(Vec<crate::Val>, crate::Val) -> crate::Val + 'static> std::fmt::Debug for Builtin<F> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "<builtin {:?}>", self.name)
	}
}

fn cond(args: crate::Val) -> crate::Val {
	args.downcast_ref::<crate::list::List>()?;

	let mut current = 0;
	let len = args.len().get_num().unwrap() as usize;
	loop {
		if current == len { return crate::nil::get() }
		if current + 1 == len { return args.index_int(current) }
		if args.index_int(current).to_bool()?.get_bool().unwrap() {
			return args.index_int(current + 1)
		}
		current += 2;
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn get(key: &str) -> crate::Val {
		BUILTINS.iter()
			.find(|p| p.0 == key)
			.map(|p| p.1())
			.unwrap_or_else(|| crate::err::Err::new(format!("No global {:?}", key)))
	}

	#[test]
	#[should_panic(expected="Baby\\'s first error")]
	fn panic() {
		let v = crate::eval("<str>", r###"
			{
				msg = "Baby's first" + " error"
				boom = panic:msg
			}.boom
		"###);
		println!("Returned value: {:?}", v);
	}

	#[test]
	fn assert_once_once() {
		let v = get("_testing_assert_cache_eval");
		assert_eq!(v.call(crate::Val::new_atomic(5.1)).get_num(), Some(5.1));
	}

	#[test]
	#[should_panic(expected="Called twice")]
	fn assert_once_twice() {
		let v = get("_testing_assert_cache_eval");
		assert_eq!(v.call(crate::Val::new_atomic(5.1)).get_num(), Some(5.1));
		assert_eq!(v.call(crate::Val::new_atomic(5.1)).get_num(), Some(5.1));
	}
}
