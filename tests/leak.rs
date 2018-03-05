extern crate ecl;
extern crate procinfo;

mod utils;

fn main() {
	utils::test_dir("lines", "ecl", |path| {
		let testcase = |_| {
			ecl::eval_file(&path.to_string_lossy()).eval().unwrap();
		};
		
		(0..1).for_each(&testcase);
		
		let statm_pre = procinfo::pid::statm_self().unwrap();
		(0..100).for_each(&testcase);
		let statm_post = procinfo::pid::statm_self().unwrap();
		
		// Surprisingly this appears to be predictable enough.
		assert_eq!(statm_pre.size, statm_post.size);
	})
}
