(* special constants - Definition v3 page 3 *)

(*$SCon : SCON*)

functor SCon(): SCON =
struct
  datatype scon = INTEGER of int | STRING of string | REAL of real
    | WORD of int | CHAR of int

  (*INTEGER < STRING < REAL < WORD < CHAR:*)
  fun ord (INTEGER _) = 0
    | ord (STRING _) = 1
    | ord (REAL _) = 2
    | ord (WORD _) = 3
    | ord (CHAR _) = 4
  fun lt (INTEGER i1, INTEGER i2) = i1 < i2
    | lt (STRING s1,  STRING s2)  = s1 < s2
    | lt (REAL r1,    REAL r2)    = r1 < r2
    | lt (WORD i1,    WORD i2)    = i1 < i2
    | lt (CHAR i1,    CHAR i2)    = i1 < i2
    | lt (scon1,      scon2)      = ord scon1 < ord scon2

  fun pr_scon(INTEGER i) = Int.string i
   |  pr_scon(WORD i) = Int.string i
   |  pr_scon(STRING s) = String.string s
   |  pr_scon(CHAR i) = "#\"" ^ chr i ^ "\""
   |  pr_scon(REAL r) = Real.string r
end;
