(* Finite maps with equality *)

(*$SortedFinMap: REPORT PRETTYPRINT SORTED_FINMAP *)

functor SortedFinMap(structure Report: REPORT
		     structure PP: PRETTYPRINT
		    ): SORTED_FINMAP =
  struct

    open Edlib

    type (''a, ''b) map = (''a * ''b) list

    val empty = []

    fun singleton p = [p]

    fun isEmpty nil = true | isEmpty _ = false

    fun lookup [] x = NONE
      | lookup ((x,y)::rest) x' =
	if x=x' then SOME(y) else lookup rest x'

    fun add (op <) (x , y , nil) = [(x, y)]
      | add (op <) (x', y', (x, y) :: rest) = 
	  if x=x' then (x', y') :: rest
	  else if x < x' then (x, y) :: add (op <) (x', y', rest)
	  else (x', y') :: (x, y) :: rest

    fun plus (op <) (l, []) = l
      | plus (op <) (l, (x, y) :: tl) = plus (op <) (add (op <) (x, y, l), tl)

    fun equal f (L1, L2) =
      case (L1, L2)
	of (nil, nil) => true
	 | (nil, _) => false
	 | (_, nil) => false
	 | ((a, b) :: rest, (a', b') :: rest') =>
	     (a = a') andalso f(b, b') andalso equal f (rest, rest')

    fun mergeMap folder map1 map2 =
      let
	fun insert(x', y', nil) = [(x', y')]
	  | insert(x', y', (x, y) :: rest) =
	      if x = x' then (x, folder(y, y')) :: rest
	      else (x, y) :: insert(x', y', rest)
      in
	List.foldL (fn (x, y) => fn m => insert(x, y, m)) map1 map2
      end

    val domSORTED   : ('a, 'b) map -> 'a list = fn x => map #1 x
    val rangeSORTED : ('a, 'b) map -> 'b list = fn x => map #2 x

    fun matches f map =
      let
	fun iter(i, x :: xs) = f(i, x) andalso iter(i + 1, xs)
	  | iter(_, nil) = true
      in
	iter(0, domSORTED map)
      end

    fun composemap (f:'b -> 'c) (m: (''a, 'b)map): (''a, 'c)map = 
	map (fn(x,y)=>(x, f y)) m

    fun fold (f : ('a * 'b) -> 'b) (x : 'b) (m : (''d,'a) map) : 'b = 
	List.foldL (fn (a, b) => fn c => f(b, c)) x m

    fun Fold (f : ((''a * 'b) * 'c) -> 'c) (x : 'c) (m : (''a,'b) map) : 'c =
	List.foldL (fn (a, b) => fn c => f((a, b), c)) x m

    type StringTree = PP.StringTree
    fun layoutMap {start, eq, sep, finish} layoutDom layoutRan m =
      let
	fun doit(x, y) = PP.NODE{start="", finish="", indent=0,
				 childsep=PP.RIGHT eq,
				 children=[layoutDom x, layoutRan y]
				}
      in
	PP.NODE{start=start, finish=finish, indent=0,
		childsep=PP.RIGHT sep, children=map doit m
	       }
      end

   (* Since SortedFinMaps are represented as sorted lists, we don't need an
      ordering relation in order to report them in sorted order. CF.
      FINMAP. *)

    type Report = Report.Report
    fun reportMapSORTED f m = Report.flatten(map f m)
  end;
