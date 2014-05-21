(*MONO_ARRAY.sml*)

signature MONO_ARRAY = sig
  eqtype array
  eqtype elem
  structure Vector : MONO_VECTOR
  val maxLen : int 
  val array : (int * elem) -> array 
  val fromList : elem list -> array 
  val tabulate : (int * (int -> elem)) -> array 
  val length : array -> int 
  val sub : (array * int) -> elem 
  val update : (array * int * elem) -> unit 
  val extract : (array * int * int option) -> Vector.vector 
  val copy : {src : array, si : int, len : int option, dst : array, di : int} -> unit 
  val copyVec : {src : Vector.vector, si : int, len : int option, dst : array, di : int} -> unit 
  val appi : ((int * elem) -> unit) -> (array * int * int option) -> unit 
  val app : (elem -> unit) -> array -> unit 
  val foldli : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
  val foldri : ((int * elem * 'b) -> 'b) -> 'b -> (array * int * int option) -> 'b 
  val foldl : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
  val foldr : ((elem * 'b) -> 'b) -> 'b -> array -> 'b 
  val modifyi : ((int * elem) -> elem) -> (array * int * int option) -> unit 
  val modify : (elem -> elem) -> array -> unit 
end; (*signature MONO_ARRAY*)

(* Type [array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type elem.  Arrays a1 and a2
   are equal if both were created by the same call to a primitive (array0,
   array, tabulate, fromList).

   All operations are as for Array.array.
*)
