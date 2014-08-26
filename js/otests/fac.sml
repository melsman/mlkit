(* File fac.sml: Simple factorial example with IntInf.
 * Copyright (c) 2014, Martin Elsman.
 * MIT License.
 *)

fun loop (n,acc) : IntInf.int =
  if n = 0 then acc
  else loop(n-1,n*acc)

fun fac n =
  print ("fac(" ^ IntInf.toString n ^ ") = " ^
         IntInf.toString (loop(n,1)) ^ "\n")

val () = List.app fac [10,20,30,300]
