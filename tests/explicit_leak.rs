extern crate ecl;
extern crate procinfo;

mod utils;

fn main() {
	utils::test_dir("lines", "ecl", |path| {
		let testcase = |_| {
			ecl::eval_file(&path.to_string_lossy()).eval().unwrap();
		};

		(0..10).for_each(&testcase);

		let statm_pre = procinfo::pid::statm_self().unwrap();
		(0..100).for_each(&testcase);
		let statm_post1 = procinfo::pid::statm_self().unwrap();
		(0..100).for_each(&testcase);
		let statm_post2 = procinfo::pid::statm_self().unwrap();

		// Surprisingly this appears to be predictable enough.
		assert!(
			statm_pre.size >= statm_post1.size ||
			statm_post1.size >= statm_post2.size,
			"Leaking memory: {} {} {}",
			statm_pre.size, statm_post1.size, statm_post2.size);
	})
}
