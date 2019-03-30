local template = {
	foo = 1
	assert foo == 2
}

impl = template: {
	foo = 2
}
