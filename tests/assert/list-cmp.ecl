assert [] == []
assert [1] == [1]
assert [1 "a" false nil] == [1 "a" false nil]
assert [1] != [2]
assert [1] != [nil]

assert [1] < [2]
assert [1] < [1 2]
assert [1 1] < [1 9]
assert [2 9] < [3 1]
assert [2 9] < [3 8]
assert [3 4] > [1 2]
assert [3 9] > [1 2]
