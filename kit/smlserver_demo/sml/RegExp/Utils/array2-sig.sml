(* array2-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Two-dimensional arrays.
 *)

signature ARRAY2 =
  sig
    type 'a array2

    val array : (int * int * 'a) -> 'a array2
	(* array(n,m,x) creates an n*m array initialized to x.
	 * Raises Size, if m or n is < 0.
	 *)
    val tabulate : (int * int * ((int * int) -> 'a)) -> 'a array2
	(* tabulate(n,m,f) creates an n*m array, where the (i,j) element
	 * is initialized to f(i,j).  Raises Size, if m or n is < 0.
	 *)
    val sub : ('a array2 * int * int) -> 'a
	(* sub(a,i,j) returns the (i,j) element. Raises Subscript if i or j
	 * is out of range.
	 *)
    val update : ('a array2 * int * int * 'a) -> unit
	(* update(a,i,j,x) sets the (i,j) element to x. Raises Subscript if
	 * i or j is out of range.
	 *)
    val dimensions : 'a array2 -> (int * int)
	(* return the size of the array *)
    val row : ('a array2 * int) -> 'a Array.array
	(* project a row of the array. *)
    val column : ('a array2 * int) -> 'a Array.array
	(* project a column of the array. *)

  end (* ARRAY2 *)

