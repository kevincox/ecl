bool = {
	false_ = false
	true_ = true
}
cond = {
	multi_found = cond:[ false "skip" false "ignore" true "result" "else" ]
	multi_default = cond:[ false "skip" false "ignore" "else" ]
	simple_true = cond:[ 1 == 1 49 28 ]
	simple_false = cond:[ 1 == 5 9 2 ]
}
nil_ = nil
reverse_list = reverse: [1 2 3]
reverse_list_empty = reverse: []
