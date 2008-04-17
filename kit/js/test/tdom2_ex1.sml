(*
signature WIDGET = sig
  structure H : TDOM2
  type color = string
  datatype corner = NW | NE | SW | SE 
  val bgcolor : color -> 
  val box : {color:color,corners:corner list} -> H.blk -> H.blk
  val width  : sz -> H.blk -> H.blk
  val height : sz -> H.blk -> H.blk
  val || : 
end
*)
local
fun ppMouse (x,y) = 
    Int.toString x ^ " - " ^ Int.toString y

open RWP
open TDom infix & nonfix div

fun f 0 = nil
  | f n = Int.toString n :: f(n-1)

val mmB = arr ppMouse (mouse())

val tB = timer 500
val aB = arr (fn x => IntInf.toInt(IntInf.mod(Time.toSeconds x, 10))) tB
(*val mB = arr (fn x => IntInf.toInt(IntInf.mod(Time.toMilliseconds x, 100))) tB *)
val bB : string list b = arr f aB
(*
val collapse : 'a b b -> 'a b
val collapse : 'a b list b -> 'a list b
val unlist : 'a list b -> 'a b list       <<<==== does not make sense
val map : ('a b -> 'b b) -> 'a list b -> 'b list b
val fold : ('a b * 'c -> 'c) -> 'c -> 'a list b -> 'c
*)
fun map (f: ''a b -> ''b b) (l:''a list b) : ''b list b =
  let fun g (xs : ''a list) : ''b b list = List.map (f o const) xs
  in 
    flatten(arr (list o g) l)
  end

infix &
fun fold (f: ''a b -> ''b b) (op & : ''b b * ''b b -> ''b b) (e:''b b) (l: ''a list b) : ''b b =
  let fun g (nil : ''a list) : ''b b = e
        | g (y::xs) = 
          List.foldl (fn (x,a) => f(const x) & a) (f(const y)) xs
  in 
    flatten(arr g l)
  end
    
fun cB() = ul (fold (li o $) (op &) (li($(const"No items"))) bB)

(*
fun pos(x,y) = 
    ("\"position: absolute; left: " ^ Int.toString x ^ "px; top: " 
     ^ Int.toString y ^ "px; height: 10px; width: 10px; padding: 1em\"")

val d = diva (Attrs[("style",arr pos (mouse()))]) ($(const "hello"))
*)

val a = h1($(arr Time.toString tB)) 
      & h1($mmB)
      & table (tr (td (cB()) & th(cB()))) 
      & hr
(* & d *)

fun toPx i = Int.toString i ^ "px"

fun mkBox c e =
    diva [S("float", const "left"),S("background", const c)]
    (diva [S("background", const "url(ul.gif) no-repeat top left")]
     (diva [S("background", const "url(ur.gif) no-repeat top right")]
      (diva [S("background", const "url(ll.gif) no-repeat bottom left")] 
       (diva [S("background", const "url(lr.gif) no-repeat bottom right")] 
         e))))

fun mkBox' c e =
    mkBox c (diva [S("padding", arr (fn x => let val p = toPx (4*x)
                                             in String.concat[p," ",p," ",p," ",p]
                                             end) aB)] 
                  e)

val h = html(const "Hej", 
         bodya [S("fontFamily", const "arial, sans-serif")] 
          (a & table (tr(td(mkBox' "#e5ecf9" ($mmB)) & td(mkBox "#05ecf9" ($mmB))))))

in
val _ = install h
end
