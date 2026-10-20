(* A copied list's destination can alias data hidden in its element type.
 * Both local and imported polymorphic calls must preserve that data (#248). *)
datatype t = Box of t list | Leaf

fun check (name,true) = ()
  | check (name,false) = raise Fail name

val () = check ("imported copy",
                case Box (copy [Box [Leaf]]) of
                    Box [Box [Leaf]] => true
                  | _ => false)

fun localCopy [] = []
  | localCopy (x::xs) = x :: localCopy xs

val () = check ("local copy",
                case Box (localCopy [Box [Leaf]]) of
                    Box [Box [Leaf]] => true
                  | _ => false)

fun wrap xs = Box (copy [Box xs])
val () = check ("formal region parameter",
                case wrap [Leaf] of Box [Box [Leaf]] => true | _ => false)

val () = check ("Basis reverse",
                case Box (List.rev [Box [Leaf]]) of
                    Box [Box [Leaf]] => true
                  | _ => false)

datatype f = FBox of (unit -> f list) list | FLeaf
val () = check ("function type argument",
                case let val leaves = [FLeaf]
                     in FBox (copy [fn () => leaves])
                     end of
                    FBox [get] => (case get () of [FLeaf] => true | _ => false)
                  | _ => false)

val () = check ("scalar type argument",
                case copy [1,2,3] of [1,2,3] => true | _ => false)
val () = print "OK\n"
