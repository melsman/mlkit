(* Finite maps with equality *)

(*$SORTED_FINMAP *)

signature SORTED_FINMAP =
  sig
    eqtype ('a, 'b) map

    val empty: (''a, 'b) map
    val singleton: ''a * 'b -> (''a , 'b) map

    val isEmpty: (''a, 'b) map -> bool

    val lookup: (''a, 'b) map -> ''a -> 'b Option

    val add:
      (''a * ''a -> bool) -> (''a * 'b * (''a, 'b) map) -> (''a, 'b) map

    val plus:
      (''a * ''a -> bool) -> (''a, 'b) map * (''a, 'b) map -> (''a, 'b) map

    val equal: ('b * 'b -> bool) -> ((''a, 'b) map * (''a, 'b) map) -> bool
	(* Equality predicate for when the range doesn't admit equality. *)

    val domSORTED: (''a, 'b) map -> ''a list
    val rangeSORTED: (''a, 'b) map -> 'b list
    val composemap: ('b -> 'c) -> (''a, 'b) map -> (''a, 'c) map
    val fold: (('a * 'b) -> 'b) -> 'b -> (''d, 'a) map -> 'b
    val Fold: (((''a * 'b) * 'c) -> 'c)-> 'c -> (''a, 'b) map -> 'c

   (* matches: applies a predicate to each element of the range, c/w
      index (counting from 0). *)

    val matches: (int * 'a -> bool) -> ('a, 'b) map -> bool

   (* mergeMap: merges two finite maps, with a composition function to apply
      to the codomains of domains which clash. *)

    val mergeMap:
      (('b * 'b) -> 'b) -> (''a, 'b) map -> (''a, 'b) map -> (''a, 'b) map

    type StringTree
    val layoutMap: {start: string, eq: string, sep: string, finish: string}
      		   -> (''a -> StringTree)
		   -> ('b -> StringTree)
		   -> (''a , 'b) map
		   -> StringTree

    type Report
    val reportMapSORTED: (''a * 'b -> Report) -> (''a, 'b) map -> Report
  end;
