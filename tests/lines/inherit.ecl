base = {
	name = "default-name"
	args = {
		service-name = name
	}
}

greeter = base:{
	name = "greeter"
	args = {
		greeting = "Hello"
		name = "World"
	}
}

hello = {
	name = "hello"
	args = base.args:{
		greeting = "Hello"
		name = "World"
	}
}

multi-level0 = { item = "an item" }
multi-level1 = multi-level0:{}
multi-level2 = multi-level1:{}
