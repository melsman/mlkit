
let
fun gen_ref _ = ref 0.0;

fun inc_ref r = (r := !r+1.0;())

fun pp_ref r = Real.toString (!r) ^ " "

fun ref_table () = Array.tabulate (100,gen_ref)

fun inc_table t = Array.app  inc_ref t

fun pp_table t = Array.app (print o pp_ref) t

fun loop f 0 = ()
  | loop f n = (f (); loop f (n-1))

val (t1,t2,t3) = (ref_table(),ref_table(),ref_table())

fun f () =
  (inc_table t1; inc_table t2; inc_table t3)

val _ = 
  loop f 100000
in  
 pp_table t2  
end  
