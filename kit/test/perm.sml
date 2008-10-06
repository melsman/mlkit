datatype p = P | A of p

fun prj 0 = NONE
  | prj n = prj n

(*
fun ccomposePartial (g,f) x =
    case g x 
     of NONE => NONE
      | SOME y => NONE
*)

(* findSome : ('a -> 'b option) -> 'a permuteList -> 'b option *)
fun findSome f = 
    composePartial 
        (fn (x, xs) =>
	    let val fx = f x 
            in if isSome fx then fx else findSome f xs 
            end, 
         prj)
    
