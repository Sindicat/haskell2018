g a = a^2

f::(a->a)->Integer->(a->a)
f g n | n <= 0 = error "The number must be strictly positive"
	    | n == 1 = g
	    | n > 1 = g . f g (n-1)
