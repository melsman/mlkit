(* Finite maps *)

signature FINMAPEQ =
  sig
    type ('a, 'b) map

    val empty : ('a, 'b) map
    val singleton : 'a * 'b -> ('a , 'b) map

    val isEmpty: ('a, 'b) map -> bool

    val lookup  : ('a * 'a -> bool) -> ('a, 'b) map -> 'a -> 'b option

    val add        : ('a * 'a -> bool) -> ('a * 'b * ('a, 'b) map) -> ('a, 'b) map
    val plus       : ('a * 'a -> bool) -> ('a, 'b) map * ('a, 'b) map -> ('a, 'b) map
    val remove     : ('a * 'a -> bool) -> 'a * ('a, 'b) map -> ('a, 'b) map option
    val dom        : ('a * 'a -> bool) -> ('a, 'b) map -> 'a Set.Set
    val range      : ('a, 'b) map -> 'b list
    val list       : ('a, 'b) map -> ('a * 'b) list
    val composemap : ('b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
    val ComposeMap : ('a * 'b -> 'c) -> ('a, 'b) map -> ('a, 'c) map
    val fold       : (('a * 'b) -> 'b) -> 'b -> ('d, 'a) map -> 'b
    val Fold       : ((('a * 'b) * 'c) -> 'c)-> 'c -> ('a, 'b) map -> 'c
    val filter     : ('a * 'b -> bool) -> ('a, 'b) map -> ('a, 'b) map

   (* mergeMap: merges two finite maps, with a composition function to apply
      to the codomains of domains which clash. *)

    val mergeMap: ('a * 'a -> bool) ->
                  (('b * 'b) -> 'b) -> ('a, 'b) map -> ('a, 'b) map -> ('a, 'b) map

    type StringTree
    val layoutMap : {start: string, eq: string, sep: string, finish: string} ->
      ('a -> StringTree) -> ('b -> StringTree) -> ('a , 'b) map -> StringTree

    type Report
    val reportMap: ('a * 'b -> Report) -> ('a, 'b) map -> Report
    val reportMapSORTED: ('a * 'a -> bool)
			 -> ('a * 'b -> Report)
			 -> ('a, 'b) map
			 -> Report
  end;
