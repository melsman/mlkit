infix 6 -
infix 4 <
datatype tree = A of int * string | B | C of tree * tree
fun retain__noinline(t:tree,n:int) =
  if n < 1 then t else retain__noinline(t,n-1)
val value = C(A(17,"x"),B)
val result = retain__noinline(value,100)
val () = prim("printStringML",case result of C(A(17,_),B)=>"OK\n" | _=>"BAD\n")
