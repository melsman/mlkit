infixr 5 ::
infix 6 + -
infix 4 <
fun !(x: 'a ref): 'a = prim("!", x)
fun eq(a:word,b:word):bool = prim("__equal_word64ub",(a,b))
fun check b = if b then () else prim("exit",1:int)
