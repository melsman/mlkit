datatype exp = LET of dec * exp | ADD of exp * exp | INT of {value:int}
     and dec = VALBIND of string * exp
