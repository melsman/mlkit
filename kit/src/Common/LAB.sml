(* Labels - Definition v3 page 4 *)

(*$LAB*)
signature LAB =
  sig
    eqtype lab

    val mk_IdentLab: string -> lab
    val mk_IntegerLab: int -> lab	(* NEW PARSER *)

    val < : lab * lab -> bool
    val is_LabN: lab * int -> bool	(* Needed when examining records
					   for tupleness. *)

    val pr_Lab: lab -> string

    val pu : lab Pickle.pu
  end;
