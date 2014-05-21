(* Test to see if optimization of argument unboxing works ok *)

fun do_list i = 
  case i
    of 1 => (1,1)
     | _ => do_list 1

fun doit (a:int,b:int) () = doit (do_list 1) ()

val _ = print "Ok\n"