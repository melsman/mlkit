(* Test the RegExp package from SML/NJ *)

fun test_regexp r s = print (s ^ " match pattern " ^ r ^ ": " ^ (Bool.toString (RegExp.regexecBoolS r s)) ^ "\n")

val _ = List.app (test_regexp "[a-zA-ZA0-9ÆØÅaæøå '\\-]+") ["???abs???", "abs???", "???abs", "abs"]
val _ = List.app (test_regexp "^[a-zA-ZA0-9ÆØÅaæøå '\\-]+$") ["???abs???", "abs???", "???abs", "abs"]
val _ = List.app (test_regexp "^[a-zA-ZA0-9ÆØÅaæøå '\\-]*$") ["???abs???", "abs???", "???abs", "abs"]
val _ = List.app (test_regexp "[a-zA-ZA0-9ÆØÅaæøå '\\-]*") ["???abs???", "abs???", "???abs", "abs"]


fun chk_dateDDMMYY s =
  (case RegExp.regexecLS "^([0-9][0-9])([0-9][0-9])([0-9][0-9])$" s of
     SOME([all,dd, mm, yy]) => print ("Date found (" ^ all ^ "(" ^ dd ^ "/" ^ mm ^ "-" ^ yy ^ ")")
  | SOME _ => print "Error 1"
  | NONE => print "Error 2")

val _ = print ("\nMT: " ^ (RegExp.pp_mt "^([0-9]([0-9]))([0-9][0-9])([0-9]([0-9]))$" "291270") ^ "\n")

val _ = chk_dateDDMMYY "29/12-70"
val _ = chk_dateDDMMYY "291270"

