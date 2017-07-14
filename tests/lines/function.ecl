local var = 5
add-two = (->n n + 2): 5
destructure-dict = (->{ a b } a + b): { a=1 b=2 }
destructure-dict-closure = (->{ a=2 } a + var): { }
destructure-dict-default = (->{ a c=3 b } a + b + c): { a=1 b=2 }
destructure-dict-override = (->{ a b c=5 } a + b + c): { a=1 b=2 c=6 }
destructure-dict-default-nil = (->{ a b=nil } a + b): { a=1 b=2 }
