(* special constants - Definition v3 page 3 *)

functor SCon(): SCON =
struct

  datatype scon = INTEGER of int | STRING of string | REAL of string
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

  fun pr_scon(INTEGER i) = Int.toString i
   |  pr_scon(WORD i) = Int.toString i
   |  pr_scon(STRING s) = "\"" ^ String.toString s ^ "\""
   |  pr_scon(CHAR i) = "#\"" ^ str(chr i) ^ "\""
   |  pr_scon(REAL r) = r

  fun eq (INTEGER i1, INTEGER i2) = i1 = i2
    | eq (WORD w1, WORD w2) = w1 = w2
    | eq (STRING s1, STRING s2) = s1 = s2
    | eq (CHAR c1, CHAR c2) = c1 = c2 
    | eq (REAL r1, REAL r2) = (r1 = r2)
    | eq _ = false

end;
