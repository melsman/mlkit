(* special constants - Definition v3 page 3 *)

(*$SCon : SCON*)

functor SCon(): SCON =
struct
  datatype scon = INTEGER of int | STRING of string | REAL of real

  fun pr_scon(INTEGER i) = Int.string i
   |  pr_scon(STRING s) = String.string s
   |  pr_scon(REAL r) = Real.string r
end;
