(* Finite maps *)

functor FinMapEq(structure Report: REPORT
		 structure PP: PRETTYPRINT
	      ): FINMAPEQ =
  struct

    type ('a, 'b) map = ('a *  'b) list

    val empty = []

    fun singleton p = [p]

    fun isEmpty nil = true | isEmpty _ = false

    fun lookup eq [] x  = NONE
      | lookup eq ((x,y)::rest) x' =
	if eq(x,x') then SOME(y) else lookup eq rest x'

    fun isin eq (x,[]) = false
      | isin eq (x,(x',y)::rest) = eq(x,x') orelse isin eq (x,rest)

    fun ins eq (x, y, nil) = [(x, y)]
      | ins eq (x', y', (p as (x, y)) :: rest) = 
	  if eq(x,x') then (x', y') :: rest
	  else p :: ins eq (x', y', rest)

    fun add eq (x, y, l) = 
        if isin eq (x,l) then ins eq (x,y,l)
        else (x,y)::l

    fun plus eq (l, []) = l
      | plus eq (l, (x, y) :: tl) = plus eq (add eq (x, y, l), tl)

    fun remove eq (x,[]) = NONE
      | remove eq (x,((x',y)::xs)) = if eq(x, x') then SOME xs
				     else case remove eq (x,xs)
					    of NONE => NONE
					     | SOME xs' => SOME ((x',y)::xs')

    fun mergeMap eq folder [] map2 = map2
      | mergeMap eq folder map1 [] = map1
      | mergeMap eq folder map1 map2 =
      let
	fun insert (x', y', nil) = [(x', y')]
	  | insert (x', y', (x, y) :: rest) =
	      if eq(x,x') then (x, folder(y, y')) :: rest
	      else (x, y) :: insert(x', y', rest)
      in
	foldl (fn ((x, y), m) => insert(x, y, m)) map1 map2
      end

    fun dom eq (m: ('a, 'b) map) = Set.fromList (fn a => fn b => eq(a,b)) (map #1 m)
    val range : ('a, 'b) map -> 'b list  = fn x => map #2 x
    val list : ('a, 'b) map -> ('a * 'b) list  = fn x => x

    fun composemap (f: 'b -> 'c) (m: ('a, 'b) map): ('a, 'c) map = 
	map (fn (a, b) => (a, f b)) m

    fun ComposeMap (f: 'a * 'b -> 'c) (m: ('a, 'b) map): ('a, 'c) map =
        map (fn (a, b) => (a, f(a, b))) m

    fun fold (f : ('a * 'b) -> 'b) (x : 'b) (m : ('d,'a) map) : 'b = 
	foldl (fn ((a, b), c) => f(b, c)) x m

    fun Fold (f : (('a * 'b) * 'c) -> 'c) (x : 'c) (m : ('a,'b) map) : 'c =
	foldl (fn ((a, b), c) => f((a, b), c)) x m

    val filter = List.filter

    type Report = Report.Report

    fun reportMap f m = Report.flatten(map f m)

    fun reportMapSORTED lt f m =
      reportMap f (ListSort.sort (fn (a, _) => fn (b, _) => lt(a, b)) m)

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
  end;
