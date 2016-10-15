local a = 5
b = 8
c = {
	d = c
	c = a
	e = b
}
d = [ c.d c.c c.e ]
