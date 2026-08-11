signature SIZE =
sig
  type 'a sz

  (* Primitives *)
  val int    : int sz
  val bool   : bool sz
  val char   : char sz
  val word   : word sz
  val real   : real sz
  val unit   : unit sz
  val string : string sz

  (* Combinators *)
  val list   : 'a sz -> 'a list sz
  val option : 'a sz -> 'a option sz
  val tup2   : 'a sz -> 'b sz -> ('a * 'b) sz
  val tup3   : 'a sz -> 'b sz -> 'c sz -> ('a * 'b * 'c) sz

  val size   : 'a sz -> 'a -> int
end
