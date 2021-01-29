structure StringMap :>
sig
  type dom = string
  type 'a map
  val empty      : 'a map
  val singleton  : dom * 'a -> 'a map
  val isEmpty    : 'a map -> bool
  val lookup     : 'a map -> dom -> 'a option
  val add        : dom * 'a * 'a map -> 'a map
  val list       : 'a map -> (dom * 'a) list
  val Fold       : ((dom * 'a) * 'b -> 'b) -> 'b -> 'a map -> 'b
end =
struct
type dom = string
type 'a map = (string * 'a) list
val empty = nil
fun singleton (p:dom*'a) : 'a map = [p]
fun isEmpty nil = true
  | isEmpty _ = false
fun lookup nil a = NONE
  | lookup ((k,v)::rest) a = if k = a then SOME v else lookup rest a
fun add (k,v,m) = (k,v)::m
fun list m = m
fun Fold f a m = List.foldl f a m
end
