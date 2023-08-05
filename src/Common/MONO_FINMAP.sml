(* Finite maps with explicit domain type *)

signature MONO_FINMAP =
  sig
    type dom
    type 'b map

    val empty      : 'b map
    val singleton  : dom * 'b -> 'b map
    val isEmpty    : 'b map -> bool
    val lookup     : 'b map -> dom -> 'b option
    val add        : dom * 'b * 'b map -> 'b map
    val plus       : 'b map * 'b map -> 'b map
    val remove     : dom * 'b map -> 'b map option
    val dom        : 'b map -> dom list
    val range      : 'b map -> 'b list
    val list       : 'b map -> (dom * 'b) list
    val fromList   : (dom * 'b) list -> 'b map
    val composemap : ('b -> 'c) -> 'b map -> 'c map
    val ComposeMap : (dom * 'b -> 'c) -> 'b map -> 'c map
    val fold       : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val Fold       : (((dom * 'b) * 'c) -> 'c)-> 'c -> 'b map -> 'c
    val filter     : (dom * 'b -> bool) -> 'b map -> 'b map

    val addList : (dom * 'b) list -> 'b map -> 'b map
      (* addList l m; adds a list of associations to a map. *)

    val mergeMap : (('b * 'b) -> 'b) -> 'b map -> 'b map -> 'b map
      (* mergeMap f m1 m2; merges two finite maps, with a composition
         function to apply to the codomains of domains which clash. *)

    exception Restrict of string
    val restrict : (dom -> string) * 'b map * dom list -> 'b map
      (* raises exception Restrict if an element
         of the list is not in the domain of the map. *)

    val enrich : ('b * 'b -> bool) -> ('b map * 'b map) -> bool
      (* enrich en (A, B) returns true if for all a and b
         such that b \in B and a \in (A \restrict dom(B))
	 we have en(a,b). *)

    type StringTree = PrettyPrint.StringTree
    val layoutMap : {start: string, eq: string, sep: string, finish: string} ->
      (dom -> StringTree) -> ('b -> StringTree) -> 'b map -> StringTree

    type Report = PrettyPrint.Report
    val reportMap: (dom * 'b -> Report) -> 'b map -> Report

    val pu : dom Pickle.pu -> 'a Pickle.pu -> 'a map Pickle.pu
    val puNoShare : dom Pickle.pu -> 'a Pickle.pu -> 'a map Pickle.pu
  end
