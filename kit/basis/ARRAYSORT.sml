(* Arraysort -- Quicksort for arrays, from SML/NJ library 
 *           -- modified for the MLKit, 2001-06-07 *)
           
signature ARRAYSORT =
  sig
    val sort   : ('a * 'a -> order) -> 'a Array.array -> unit
    val sorted : ('a * 'a -> order) -> 'a Array.array -> bool
  end

(* 
   [sort ordr arr] sorts array arr in-place, using ordering relation ordr.

   [sorted ordr arr] returns true if the elements of array arr is
   appear in (weakly) increasing order, according to ordering ordr.
*)
