local a = {
	b = {
		f = ->a a + a
		i = 5
	}
}

index = a.b.f:a.b.i
math = a.b.f:4 + a.b.f:5 + a.b.f:6

cmp-eq-false = "1" == 1
cmp-eq-true = 1 == 1
cmp-ge-eq = 3 >= 2
cmp-ge-false = 1 >= 2
cmp-ge-true = 1 >= -0.2
cmp-gt-false = 0.1 > 0.2
cmp-gt-true = 0.2 > 1
cmp-le-eq = -1.2 <= 1.2
cmp-le-false = 1.2 <= -1.2
cmp-le-true = -1.2 <= 2
cmp-lt-false = 5.2 < 2.3
cmp-lt-true = 1 < 2.3
