val minInt = Option.valOf Int.minInt;
val maxInt = Option.valOf Int.maxInt;
val rminInt = real minInt;
val rmaxInt = real maxInt;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");

fun chkfail s f r =
  tst0 s ((f r; "WRONG") 
	  handle Overflow => "OK" | _ => "WRONG")

val test4c = chkfail "test4c" Real.ceil (rminInt-1.0)
(*val test4c = map (chkfail "test4c" Real.ceil) [rminInt-1.0, rmaxInt+0.1];*)

