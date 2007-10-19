signature TLIST =
  sig
    type 'a tlist = ('a list,TimeVal.B)TimeVal.t
    val ::: : (''a,TimeVal.B)TimeVal.t * ''a tlist -> ''a tlist
    val nill : unit -> ''a tlist
  end

structure TList :> TLIST =
struct

open TimeVal
type 'a tlist = ('a list,B)t

fun :::(x : (''a,B)t, xs: ''a tlist) : ''a tlist =
  arr(op ::) (pair(x,xs))

fun nill() : ''a tlist = const nil

end

structure TList__Test =
struct
open TList infix ::: open TimeVal

fun pr_list pr l =
  String.concatWith "," (map pr (current l))

val l = const 5 ::: (const 8 ::: nill())

val _ = print (pr_list Int.toString l)
end
