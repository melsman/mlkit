(*charpat.sml  09/07/1997 15:46. tho.*)

	       	     val c4 = #"\u001f" : char;
fun pr s = output (std_out, s);
fun prl s = pr (s ^ "\n");
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
  fun int_to_string n = if n < 0 then "~" ^ string' (~n) else string' n
end;

prl (implode (explode "Hallo wordl!\n"));

app (prl o int_to_string o ord) [#"a", #"A", #" ", chr 42, #"\117"];

fun isSpace #" " = true
  | isSpace #"\t" = true
  | isSpace #"\n" = true
  | isSpace _ = false
fun bool_to_string true = "true"
  | bool_to_string false = "false";
  
app (prl o bool_to_string o isSpace) (explode "Hallo wordl!\n");
