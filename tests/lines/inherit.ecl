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
