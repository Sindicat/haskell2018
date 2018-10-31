id_::a->a
id_ a = a

eval::(a->b,a)->b
eval (a,b) = a b

exchange::(a,b)->(b,a)
exchange (a,b) = (b,a)

compose::(b->c)->(a->b)->a->c
compose f a b = f $ a b

curry_::((a,b)->c)->(a->b->c)
curry_ f = c
    where c a b = f (a,b)

associate::(a,(b,c))->((a,b),c)
associate (a,(b,c)) = ((a,b),c)
