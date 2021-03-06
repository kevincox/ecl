a-base = {
	name = "base name"
	base-thing = {
		name = ..name
	}
}

a-sub = a-base: {
	same-level = { r = ..name }.r
	sub-thing = {
		bar = ..name
	}
}

local dict-dotted = {
	x = 1
	y.z = { x = ..x }
}

local x = 1

local func-dict = ->{x=..x + 1 y} {x=..x x2=...x y=..y}
func-dict-val = func-dict:{y=2}

local func-list = ->[y x=..x] {x=..x x2=...x y=..y}
func-list-val = func-list:[2]

dict-dotted-val = dict-dotted.y.z.x
local func-one = ->x { x = ..x }
func-one-val = func-one:5

local func-two-args = ->x ->y { x=...x y={y=...y}}
func-two-args-val = func-two-args:1:2
