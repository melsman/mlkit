fun print (s:string) : unit = prim("printString", "printString", s)

infix ^ 
fun (s : string) ^ (s' : string) : string = prim ("concatString", "concatStringProfiling", (s, s'))

(*fun myfun (x, y, z) = 
  let val _ = print y
      val a1 = z ^ y 
    val _ = print a1
  in a1 ^ x
  end*)

fun myfun2 (a,b,c,d,e,f,g) = print (a^b^c^d^e^f^g)


(*val s = myfun ("3", "2", "1")*)
val s2 = myfun2("1","2","3","4","5","6","7")