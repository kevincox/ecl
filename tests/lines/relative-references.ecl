base = {
	name = "base name"
	base-thing = {
		name = ..name
	}
}

sub = base: {
	same-level = { r = ..name }.r
	sub-thing = {
		bar = ..name
	}
}
