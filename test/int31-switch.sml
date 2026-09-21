(* Packed array loads must preserve negative Int31 patterns in switches. *)
val a = Int31Array.fromList [~3,~2,~1,0,1]
fun choose__noinline (x:Int31.int) =
  case x of ~3 => 33 | ~2 => 22 | ~1 => 11 | 0 => 44 | _ => 55
val () = List.app
  (fn i => print (Int.toString (choose__noinline (Int31Array.sub(a,i))) ^ "\n"))
  [0,1,2,3,4]
