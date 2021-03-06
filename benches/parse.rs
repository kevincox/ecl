#![feature(test)]

extern crate ecl;
extern crate test;

#[bench]
fn parse_list(b: &mut test::Bencher) {
	b.iter(||
		::ecl::grammar::parse("<str>", r###"
			[
				1
				1 + 2
				1 + 2 + 3
				4
				5
				6
				7
				8
				9
				"str"
				true
				false
			]
		"###.chars())
	)
}

#[bench]
fn parse_dict(b: &mut test::Bencher) {
	b.iter(||
		ecl::grammar::parse("<str>", r###"
			{
				a = 1
				b = 2
				c = 3
				d = 4
				e = 5
				f = 6
				g = 7
				nested = {
					a = 1
					b = 2
					c = 3
					d = 4
				}
			}
		"###.chars())
	)
}

#[bench]
fn parse_dict_implicit(b: &mut test::Bencher) {
	b.iter(||
		ecl::grammar::parse("<str>", r###"
			a = 1
			b = 2
			c = 3
			d = 4
			e = 5
			f = 6
			g = 7
			nested = {
				a = 1
				b = 2
				c = 3
				d = 4
			}
		"###.chars())
	)
}
