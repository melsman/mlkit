fun do_regExpBool p s =
  "<code>RegExp.match</code> \"" ^ p ^ "\" \"" ^ s ^ "\" gives " ^ 
  (Bool.toString (RegExp.match (RegExp.fromString p) s)) ^ "<br>\n"

fun do_regExp p s =
  let
    fun pl' [] = ""
      | pl' [x] = "\"" ^ x ^ "\""
      | pl' (x::xs) = "\"" ^ x ^ "\", " ^ (pl' xs)
    fun pl NONE = "No Result"
      | pl (SOME l) = pl' l
  in
    "<code>RegExp.extract</code> \"" ^ p ^ "\" \"" ^ s ^ "\" gives [" ^ 
    (pl (RegExp.extract (RegExp.fromString p) s)) ^ "]<br>\n"
  end

val emailp = "([a-zA-Z][0-9a-zA-Z._]*)@([0-9a-zA-Z._]+)"
val _ =
  Page.return "RegExp examples"
  `<h4>Function <code>RegExp.match</code></h4><p>
  ^(do_regExpBool "[0-9]+" "99")
  ^(do_regExpBool "[0-9]+" "aa99AA")
  ^(do_regExpBool "[0-9]+.*" "99AA")
  ^(do_regExpBool "[0-9]+" "99AA")
  ^(do_regExpBool "[0-9]+" "aa99")

  <h4>Function <code>RegExp.extract</code></h4><p>
  ^(do_regExp "Name: ([a-zA-Z ]+);Tlf: ([0-9 ]+)" "Name: Hans Hansen;Tlf: 66 66 66 66")
  ^(do_regExp emailp "name@company.com")
  ^(do_regExp emailp "name@company@com")

  <h4>A group that takes part in a match repeatedly</h4>
  ^(do_regExpBool "(a(b+))+" "abbabbb")
  ^(do_regExp "(a(b+))+" "abbabbb")

  ^(do_regExpBool "(([a-zA-Z][0-9a-zA-Z._]*)@[0-9a-zA-Z._]+,?)*" "joe@it.edu,sue@id.edu,pat@it.edu")
  ^(do_regExp "(([a-zA-Z][0-9a-zA-Z._]*)@[0-9a-zA-Z._]+,?)*" "joe@it.edu,sue@id.edu,pat@it.edu")

  <h4>A group that does not take part in a match</h4>
  ^(do_regExp "(ab)|(cd)" "cd")
  ^(do_regExp "(ab)|(cd)" "ab")`
