bool = {
	false_ = false
	true_ = true
}
cond_ = {
	multi_found = cond:[ false "skip" false "ignore" true "result" "else" ]
	multi_default = cond:[ false "skip" false "ignore" "else" ]
	simple_true = cond:[ 1 == 1 49 28 ]
	simple_false = cond:[ 1 == 5 9 2 ]
}
dict-map = map:(->[k v] "$k=$v"):{a=1 b=2 c=3}
list-map = map:(->i i + 2):[1 2 3]
list-foldl = foldl:(->accum ->elem accum + elem):"s":["a" "b" "c"]
list-foldr = foldr:(->accum ->elem accum + elem):"s":["a" "b" "c"]
list-reverse = reverse:[1 2 3]
list-reverse-empty = reverse:[]
nil_ = nil
