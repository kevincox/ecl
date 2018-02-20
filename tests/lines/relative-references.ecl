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

local func-one = ->x { x = ..x }
func-one-val = func-one:5

local x = 1
local func-dict = ->{x=..x y} {x=..x x2=...x y=..y}
func-dict-val = func-dict:{y=2}

local func-list = ->[y x=..x] {x=..x x2=...x y=..y}
func-list-val = func-list:[2]
