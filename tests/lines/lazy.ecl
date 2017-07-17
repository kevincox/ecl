local unused = panic:"Unused local."

cond-eval = cond:[
	false panic:"False branch evaluated"
	false panic:"Second false evaluated"
	42]

local dict = { used=5 unused=panic:"Unused dict element." }
dict-eval = dict.used

local list = [ panic:"Unused list head" 6 panic:"Unused list tail" ]
list-eval = index:list:1
