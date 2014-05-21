(* vector-sig.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * The abstract interface of vectors and matrices in some dimension.
 *)

signature VECTOR =
  sig
    type 'a vec

    val dim : int	(* dimension of the vectors *)

    val tabulate : (int -> 'a) -> 'a vec

    val equal : real vec * real vec -> bool
    val zerov : real vec
    val addv : (real vec * real vec) -> real vec
    val subv : (real vec * real vec) -> real vec
    val dotvp : (real vec * real vec) -> real
    val crossvp : (real vec * real vec) -> real vec
    val addvs : (real vec * real) -> real vec
    val mulvs : (real vec * real) -> real vec
    val divvs : (real vec * real) -> real vec

    val mapv : ('a -> 'b) -> 'a vec -> 'b vec
    val map3v : (('a * 'b * 'c) -> 'd) -> ('a vec * 'b vec * 'c vec) -> 'd vec
    val foldv : ('a * 'b -> 'b) -> 'a vec -> 'b -> 'b
    val format : {lp : string, sep : string, rp : string, cvt : 'a -> string}
	  -> 'a vec -> string
    val explode : 'a vec -> 'a list
    val implode : 'a list -> 'a vec

    type matrix  (* matrices are always real valued *)

    val zerom : matrix
    val addm : (matrix * matrix) -> matrix
    val outvp : (real vec * real vec) -> matrix

  end
