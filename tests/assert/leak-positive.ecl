trace = _testing_leak_trace
one = trace: 1

assert one == 1
assert _testing_leak_count == one
