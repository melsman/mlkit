infix 1 seq
fun e1 seq e2 = e2;
fun pr_ln s s' = print (s ^ ": " ^ s' ^ "\n")

val test13d = (Word8.fromString "FFFFFFFFFFF" seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
val _ = pr_ln "test13d" test13d
