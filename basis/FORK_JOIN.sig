signature FORK_JOIN = sig
  val par    : (unit -> 'a) * (unit -> 'b) -> 'a * 'b
  val pair   : ('a -> 'c) * ('b -> 'd) -> 'a * 'b -> 'c * 'd
  val parfor : int -> int * int -> (int -> unit) -> unit
  val pmap   : ('a -> 'b) -> 'a list -> 'b list

  val alloc  : int -> 'a -> 'a array
end
