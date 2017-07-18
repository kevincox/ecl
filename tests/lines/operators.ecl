local a = {
	b = {
		f = ->a a + a
		i = 5
	}
}

index = a.b.f:a.b.i
math = a.b.f:4 + a.b.f:5 + a.b.f:6
