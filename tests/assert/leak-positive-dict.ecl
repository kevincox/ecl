dict = {
	trace = _testing_leak_trace
	r = trace: 1
}
one = dict.r

assert one == 1
assert _testing_leak_count == one
