local unused = panic:"Unused local."

cond-eval = cond:[
	false panic:"False branch evaluated"
	false panic:"Second false evaluated"
	42]

local dict = { used=5 unused=panic:"Unused dict element." }
dict-eval = dict.used

local func-raw = ->unused 5
local func-dict = ->{a=5 b=panic:"Default argument evaluated"} a + 8
func-eval = {
	dict-passed = func-dict:{ b=6 }
	
	# Right now we evaluate unused arguments. This is unlikely to be an issue.
	# dict-passed-unused = func-dict:{ b=panic:"Unused arg evaluated." }
	# dict-default = func-dict:{}
	# raw = func-raw:(panic:"Single argument evaluated")
}

local list = [ panic:"Unused list head" 6 panic:"Unused list tail" ]
list-eval = index:list:1

local template = {
	not-merged = panic: "Overridden value."
	
	# This indirection is required because we need to check if a value is a
	# dict to merge the parent and child values.
	merged = [ panic: "Overridden value." ]
}
template-eval = template:{
	not-merged = "ok"
	merged = {}
}
