
open TimeVal
open TDom
infix &

val _ = print "<html><body id='bdy'></body></html>"

val a = h1($(arr Time.toString (timer 100))) & h1($(arr (fn (x,y) => Int.toString x ^ " - " ^ Int.toString y) (mouse())))

val _ = insertDOM "bdy" a
