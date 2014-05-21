signature TLIST =
  sig
    type 'a beh = ('a,TimeVal.B)TimeVal.t
    val ::: : ''a beh * ''a list beh -> ''a list beh
    val nill : unit -> ''a list beh
(*    val sort : ''a beh list -> ''a list beh *)
  end

structure TList :> TLIST =
struct

open TimeVal
type 'a beh = ('a,TimeVal.B)TimeVal.t
type 'a tlist = 'a list beh

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
