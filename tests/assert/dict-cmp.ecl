assert {} == {}
assert {a = 1} == {a = 1}
assert {a = 1} != {b = 1}
assert {a = 1} != {a = 2}

assert {a = 1} < {a = 2}
assert {a = 1} < {b = 1}
assert {a = 1 b = 2} < {a = 1 b = 3}
assert {b = 2 a = 1} < {b = 3 a = 1}
assert {a = 2 b = 9} < {a = 3 b = 8}
assert {b = 3 c = 4} > {a = 1 b = 2}
