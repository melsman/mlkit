(* Finite maps *)

(*$ORDER_FINMAP *)
signature ORDER_FINMAP =
  sig
    type dom
    type 'b map

    val lt: dom * dom -> bool

    val empty : 'b map
    val singleton : dom * 'b ->  'b map
    val isEmpty:  'b map -> bool
    val lookup  : 'b map -> dom -> 'b Option
    val add        : dom * 'b * 'b map -> 'b map
    val plus       : 'b map * 'b map ->  'b map
    val remove     : dom * 'b map -> ('b map, string) General.Result
    val dom        : 'b map -> dom list
    val range      : 'b map -> 'b list
    val list       : 'b map -> (dom * 'b) list
    val fromList   : (dom * 'b) list -> 'b map
    val fromSortedList: (dom * 'b) list -> 'b map
    val composemap : ('b -> 'c) -> 'b map ->  'c map
    val ComposeMap : (dom * 'b -> 'c) ->  'b map -> 'c map
    val fold       : (('a * 'b) -> 'b) -> 'b -> 'a map -> 'b
    val Fold       : (((dom * 'b) * 'c) -> 'c)-> 'c ->  'b map -> 'c
    val filter     : (dom * 'b -> bool) ->  'b map -> 'b map

    val delete : dom * 'b map -> 'b map
      (* delete (k, m); deletes an association k |--> r from m, 
         returning m if not present. *) 

    val oneForWhich : ((dom * 'b) -> bool) -> 'b map -> (dom * 'b) Option 
      (* oneForWhich f m; returns (if present) an association pair 
         for which the predicate f is true. *)

    val addList : (dom * 'b) list -> 'b map -> 'b map
      (* addList l m; adds a list of associations to a map. *)

    val mergeMap : (('b * 'b) -> 'b) ->  'b map -> 'b map ->  'b map
      (* mergeMap f m1 m2; merges two finite maps, with a composition 
         function to apply to the codomains of domains which clash. *)

    exception Restrict
    val restrict : 'b map * dom list -> 'b map
      (* raises exception Restrict if an element
         of the list is not in the domain of the map. *)

    val enrich : ('b * 'b -> bool) -> ('b map * 'b map) -> bool
      (* enrich en (A, B) returns true if for all a and b 
         such that b \in B and a \in (A \restrict dom(B)) 
	 we have en(a,b). *)

    type StringTree
    val layoutMap : {start: string, eq: string, sep: string, finish: string} ->
      (dom -> StringTree) -> ('b -> StringTree) ->  'b map -> StringTree

    type Report
    val reportMap: (dom * 'b -> Report) -> 'b map -> Report
    val reportMapSORTED: (dom * 'b -> Report)
			 -> 'b map
			 -> Report
  end
