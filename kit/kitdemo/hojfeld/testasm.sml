(*testasm.sml  25/06/1997 12:49. tho.*)

val i = 42;

  local
    fun string' 0 = "0"
    |   string' 1 = "1"
    |   string' 2 = "2"
    |   string' 3 = "3"
    |   string' 4 = "4"
    |   string' 5 = "5"
    |   string' 6 = "6"
    |   string' 7 = "7"
    |   string' 8 = "8"
    |   string' 9 = "9"
    |   string' n = string' (n div 10) ^ string' (n mod 10)
  in
    fun intToString n = if n < 0 then "~" ^ string' (~n) else string' n
  end;

output (std_out, intToString i);
