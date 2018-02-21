#![feature(test)]

extern crate ecl;
extern crate test;

#[bench]
fn fib_func_rec(b: &mut test::Bencher) {
	let fib = ecl::parse("<fib-str>", r###"
		{
			fib = ->i cond:[i <= 2 1 fib:(i - 1) + fib:(i - 2)]
		}.fib
	"###).unwrap();
	let num = ecl::parse("<num-str>", "(20)").unwrap();
	let fib_num = || fib.call(num.clone()).get_num();
	
	assert_eq!(fib_num(), Some(6765.0));
	
	b.iter(fib_num)
}

#[bench]
fn fib_func_iter(b: &mut test::Bencher) {
	let fib = ecl::parse("<fib-str>", r###"
		{
			fib-part = ->i cond:[
				i <= 2 [1 1]
				{
					prev-part = fib-part:(i - 1)
					prev = index:prev-part:1
					next = index:prev-part:0 + prev
					part = [prev next]
				}.part]
			fib = ->i index:(fib-part:i):1
		}.fib
	"###).unwrap();
	let num = ecl::parse("<num-str>", "(80)").unwrap();
	let fib_num = || fib.call(num.clone()).get_num();
	
	assert_eq!(fib_num(), Some(23416728348467685.0));
	
	b.iter(fib_num)
}
