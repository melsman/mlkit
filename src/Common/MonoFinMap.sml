signature BASIC_FINMAP =
    sig
	type dom
	type 'b map
	val empty      : 'b map
	val lookup     : 'b map -> dom -> 'b option
	val add        : dom * 'b * 'b map -> 'b map
	val remove     : dom * 'b map -> 'b map option      
	val mergeMap   : (('b * 'b) -> 'b) -> 'b map -> 'b map -> 'b map
	val Fold       : (((dom * 'b) * 'c) -> 'c)-> 'c -> 'b map -> 'c
	val pu         : dom Pickle.pu -> 'a Pickle.pu -> 'a map Pickle.pu
    end

functor MonoFinMap (B : BASIC_FINMAP) 
    : MONO_FINMAP where type dom = B.dom 
		  where type 'a map = 'a B.map =
    struct
	open B
	fun singleton(d,r) = add(d,r,empty)

	fun fold f a m =
	    Fold (fn ((d,r),a) => f(r,a)) a m

	fun list m = Fold (op ::) nil m

	exception NOT_EMPTY
	fun isEmpty m = 
	    let fun f _ = raise NOT_EMPTY
	    in Fold f () m ; true
	    end handle NOT_EMPTY => false

	fun dom m = map #1 (list m)

	fun range m = map #2 (list m)

	fun addList nil m = m
	  | addList l m =
	    let fun from m nil = m
		  | from m ((d,r)::xs) = from (add(d,r,m)) xs
	    in from m l
	    end

	fun fromList l = addList l empty

	fun ComposeMap f m =
	    Fold (fn (p as (d,r),a) => add(d, f p, a)) empty m

	fun composemap f m =
	    let fun g (_,x) = f x
	    in ComposeMap g m
	    end

	fun plus (m1,m2) = mergeMap #2 m1 m2

	fun filter f m =
	    Fold (fn (p as (d,r),a) => if f p then add(d,r,a) else a)
	    empty m

	exception Restrict of string
	fun restrict (pp, m, l) =
	    let 
		fun res ([], a) = a
		  | res (d::rest, a) = 
		    case lookup m d of 
			SOME r => res(rest,add(d,r,a))
		      | NONE => raise Restrict(pp d)
	    in res (l, empty)
	    end
	
	fun enrich f (m1, m2) =
	    Fold (fn ((d2,r2),b) => 
		  case lookup m1 d2 of 
		      SOME r1 => b andalso f(r1,r2)
		    | NONE => false) true m2  
	    
	structure PP = PrettyPrint

	type StringTree = PP.StringTree
	    
	fun layoutMap {start, eq=equal, sep, finish} layoutDom layoutRan m =
	    PP.NODE {start=start,
		     finish=finish,
		     children=map (fn (d,r) => 
				   PP.NODE {start="",
					    finish="",
					    children=[layoutDom d, 
						      layoutRan r],
					    indent=3,
					    childsep=PP.RIGHT equal})
		     (list m),
		     indent=3,
		     childsep=PP.RIGHT sep}

	type Report = Report.Report

	fun reportMap f t = Report.flatten(map f (list t))
    end
