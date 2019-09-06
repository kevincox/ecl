zero = {
	trace = _testing_leak_trace
	r = trace: 0
}.r

assert zero == 0
assert _testing_leak_count == zero
