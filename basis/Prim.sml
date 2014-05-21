structure Prim =
  struct
      fun equal_int32ub(a:int,b) = a=b
      fun mod_int32ub(a:int,b,e) = a mod b handle _ => raise e
      fun div_int32ub(a:int,b,e) = a div b handle _ => raise e
      fun printStringML s = print s
  end
