datatype exp = LET of dec * exp | ADD of exp * exp | INT of int
     and dec = VALBIND of string * exp
