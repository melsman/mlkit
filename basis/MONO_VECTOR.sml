(*MONO_VECTOR.sml*)

signature MONO_VECTOR = sig
  eqtype vector
  eqtype elem
  val maxLen : int 
  val fromList : elem list -> vector 
  val tabulate : (int * (int -> elem)) -> vector 
  val length : vector -> int 
  val sub : (vector * int) -> elem 
  val extract : (vector * int * int option) -> vector 
  val concat : vector list -> vector 
  val mapi : ((int * elem) -> elem) -> (vector * int * int option) -> vector 
  val map : (elem -> elem) -> vector -> vector 
  val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit 
  val app : (elem -> unit) -> vector -> unit
  val foldli : ((int * elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  val foldri : ((int * elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
end; (*signature MONO_VECTOR*)

(* Type [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type elem.  Type vector
   admits equality, and vectors v1 and v2 are equal if they have the same
   length and their elements are equal.

   All operations are as for Vector.vector.
*)

