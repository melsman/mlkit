fun print (s:string) : unit = prim("printString", "printString", s)

val _ = 
    let
      val _ = print "Start...\n"
      datatype k = A of int * string

      val k1 = A(5,"hej")

    in
      case k1 of
	A(i,s) => (print s)  ;
	  print "End\n"
    end
