(* Labels - Definition v3 page 4 *)

(*$Lab: LAB*)
functor Lab(): LAB =
  struct

    open Edlib
    open General
      
    datatype lab = LAB of string

   (* Ordering of labels requires a rethink, because we have to be careful
      when printing tuple types and values. `Lab.<' should behave correctly
      for numeric labels (2 < 10) and still give an unambiguous ordering
      for others (a2 > a10). We'd also better be convinced that the
      ordering is transitive, or things could start going horribly wrong. *)

    val op < = fn (LAB str1, LAB str2) =>
      case (IntParse.parse str1, IntParse.parse str2)
	of (OK(i1, ""), OK(i2, "")) => Int.lt i1 i2
	 | _ => str1 < str2

    fun is_LabN(LAB str, i) =
      case IntParse.parse str
	of OK(i', "") => (i = i')
	 | _ => false

    fun pr_Lab(LAB str) = str

    val mk_IdentLab = LAB
    val mk_IntegerLab = LAB o Int.string
  end;
