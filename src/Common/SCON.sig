(* special constants - Definition v3 page 3 *)

(* I'd like two views of SCON, one with the datatype hidden, but that seems
   to cause Poly/ML problems with the local/sharing/open stuff. *)

signature SCON =
sig
  datatype scon = INTEGER of IntInf.int
  		| WORD of IntInf.int
  		| STRING of string
  		| CHAR of int
		| REAL of string

  val eq : scon * scon -> bool
  val lt : scon * scon -> bool

  val pr_scon: scon -> string
end;
