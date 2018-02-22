base = {
	adict-add.val = 2
	adict-override.val = 5
	
	dict = {
		val = 4
		use-val = val + 2
	}
	
	dict-use-val = dict.val + 3
}

sub = base:{
	adict-add = {
		new = 3
	}
	adict-override = {
		val = 4
	}
	
	dict.val = 6
}
