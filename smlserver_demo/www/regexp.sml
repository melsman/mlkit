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
  Ns.return `<html>
   <head>
   <title>RegExp examples</title>
   </head>
   <body bgcolor=white>
  <h2>Function <code>RegExp.match</code></h2><p>
  ^(do_regExpBool "[0-9]+" "99")
  ^(do_regExpBool "[0-9]+" "aa99AA")
  ^(do_regExpBool "[0-9]+.*" "99AA")
  ^(do_regExpBool "[0-9]+" "99AA")
  ^(do_regExpBool "[0-9]+" "aa99")

  <h2>Function <code>RegExp.extract</code></h2><p>
  ^(do_regExp "Name: ([a-zA-Z ]+);Tlf: ([0-9 ]+)" "Name: Hans Hansen;Tlf: 66 66 66 66")
  ^(do_regExp emailp "name@compagny.com")
  ^(do_regExp emailp "name@compagny@com")

  <h2>A group that takes part in a match repeatedly</h2>
  ^(do_regExpBool "(a(b+))+" "abbabbb")
  ^(do_regExp "(a(b+))+" "abbabbb")

  <h2>A group that does not take part in a match</h2>
  ^(do_regExp "(ab)|(cd)" "cd")
  ^(do_regExp "(ab)|(cd)" "ab")

  <hr>
  <a href="http://www.smlserver.org/">SMLserver Home Page</a> 
  (<a href="mailto:smlserver@it.edu">smlserver@it.edu</a>) 2001-08-08
   </body>
   </html>`
