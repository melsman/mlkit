type pair = {x : int, y : int}
fun f ([]:pair list) = "Hi There"
and f (xs as x::_ : pair list) = "Hi There again"
