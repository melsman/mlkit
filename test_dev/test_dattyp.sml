  infix  7  mod
  infix  6  + -
  infixr 5  ::
  infix  4  = <> > >= < <=
  infix  3  :=
  type 'a ref = 'a ref


  fun print (s:string) : unit = prim("printStringML", s)

  datatype int_list = C of int * int_list
                    | N

  fun len(N) = 0
    | len(C(i,ls)) = 1 + len ls

  fun makeList 0 = (print "in makelist 0\n";N)
    | makeList h = (print "in makelist h\n";C (h, makeList (h-1)))

  val res = makeList 1500

  val l = len res
