open Vector

fun length (t : 'a vector) : int = 
  prim ("table_size", "table_size", t)
fun sub (a : 'a vector, i : int) : 'a =
  prim ("word_sub0", "word_sub0", (a, i))

infix sub
val v1 = fromList [1,2]
val v2 = fromList [1,2]
fun eq (v1,v2) = 
  let val l1 = length v1 val l2 = length v2
      fun loop j = if j < 0 then true
		   else ((v1 sub j) = (v2 sub j) andalso loop (j-1))
  in l1 = l2 andalso loop (l1-1)
  end
val _ = if length v1 = 2 andalso length v2 = 2 then print "OK\n" else print "Err\n" 
val _ = if eq(v1,v2) then print "OK\n"
	else print "Err\n"

