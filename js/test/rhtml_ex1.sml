local
fun ppMouse (x,y) = 
    Int.toString x ^ " - " ^ Int.toString y

open RWP
open RHtml infix & nonfix div

structure RW = RWidget

fun f 0 = nil
  | f n = Int.toString n :: f(n-1)

val mmB = arr ppMouse (mouse())

val tB = timer 500
val aB = arr (fn x => IntInf.toInt(IntInf.mod(Time.toSeconds x, 10))) tB
(*val mB = arr (fn x => IntInf.toInt(IntInf.mod(Time.toMilliseconds x, 100))) tB *)
val bB : string list b = arr f aB

fun color i =
    arr (fn 0 => "blue"
          | 1 => "black"
          | 2 => "red"
          | 3 => "green"
          | 4 => "darkgreen"
          | 5 => "darkred"
          | 7 => "margenta"
          | _ => "red") i

infix &

fun cB() = ul (RW.fold (lia [S("color", color aB)] o $) (op &) (li ($(const"No items"))) bB)

val a = h1($(arr Time.toString tB)) 
      & h1($mmB)
      & cB()
      & hr

fun toPx i = Int.toString i ^ "px"

fun mkBox c e = 
    RW.box (const c) e

fun mkBox' c e =
    mkBox c (RW.pad (arr (fn x => 4*x) aB) e)

val (sB,inp) = RW.textField nil

val (bb,i) = RW.mouseOver ($(const"MouseOver"))

val i = iff (bb, i, b i)
 
val h = html(const "Hej", 
             bodya [S("fontFamily", const "arial, sans-serif")] 
                   (a 
                        & table (    tr(td(mkBox' Color.yellow ($mmB)) & td(mkBox Color.blue ($mmB)))
                                   & tr(td inp & td ($sB))
                                   & tr(td i & td ($(arr Bool.toString bb))))
                        & hr
            ))
in
val _ = install h
end
