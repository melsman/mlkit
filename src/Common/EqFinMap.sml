(* Finite maps based on equality *)

functor EqFinMap(structure Report: REPORT
		 structure PP: PRETTYPRINT
		 type dom
		 val eq : dom * dom -> bool
	        ): MONO_FINMAP =
  struct

    type dom = dom
    datatype 'b map = FM of {elts: (dom * 'b) list, unique : bool ref}
      
    local val true_ref = ref true
    in val empty = FM {elts = [], unique = true_ref}
    end

    fun singleton p = FM {elts = [p], unique = ref true}

    fun isEmpty (FM{elts=[],...}) = true
      | isEmpty _ = false

    fun lookup (FM{elts=l,...}) x = 
	let fun look [] = NONE
	      | look ((x',y)::rest) = if eq(x,x') then SOME(y) else look rest
	in look l
	end

    fun isin(x,FM{elts=l,...}) = 
	let fun look [] = false
	      | look ((x',_)::rest) = eq(x,x') orelse look rest
	in look l
	end

    fun add (x, y, FM{elts=l,...}) = FM{elts=(x,y)::l,unique=ref false}

    fun addList pairs m = foldr (fn ((d,r), m) => add(d,r,m)) m pairs

    fun fromList pairs = addList pairs empty

    fun plus (FM{elts=l1,...}, FM{elts=l2,...}) = 
	FM{elts=l2 @ l1,unique = ref false}

    fun remove (x,FM{elts=l,unique}) = 
	let val is_unique = !unique
	    fun revappend([],rest) = rest
	      | revappend(hd::tl,rest) = revappend(tl,hd::rest)
	    fun rmv ([],found,a) = 
		if found then (SOME(FM{elts=rev a,unique=ref false})) else
		    NONE
	      | rmv ((p as (x',y))::rest,found,a) = 
		if eq(x,x') then 
		    if is_unique then 
			SOME(FM{elts=revappend(a,rest),unique=ref true})
		    else rmv(rest,true,a)
		else rmv(rest,found,p::a)
	in
	    rmv(l,false,[])
	end

    fun mergeMap folder (FM{elts=[],...}) map2 = map2
      | mergeMap folder map1 (FM{elts=[],...}) = map1
      | mergeMap folder (FM{elts=map1,unique=u1}) (FM{elts=map2,unique=u2}) =
      let
	fun insert(x', y', nil) = [(x', y')]
	  | insert(x', y', (x, y) :: rest) =
	      if eq(x,x') then (x, folder(y, y')) :: rest
	      else (x, y) :: insert(x', y', rest)
      in
	FM{elts=foldl (fn ((x, y), m) => insert(x, y, m)) map1 map2,
	   unique=ref ((!u1) andalso (!u2))}
      end

    fun elimDuplicates l = 
        let val duplicates = ref false
	    fun seen(x,[]) = false
	      | seen(x,(x',_)::rest) = eq(x,x') orelse seen(x,rest)
	    fun elim((p as (x,y))::rest,a) = 
		if seen(x,a) then (duplicates := true; elim(rest,a))
		else elim(rest,p::a)
	      | elim([],a) = rev a
	in
	    (elim(l,[]), !duplicates)
	end

    fun elimDups (FM{elts,unique}) = 
	if !unique then elts else
	    let val (elts',duplicates) = elimDuplicates elts
	    in
		unique := not(duplicates);
		elts'
	    end

    fun dom m =
        let val elts' = elimDups m
	in map #1 elts'
	end

    fun range m = 
	let val elts' = elimDups m
	in map #2 elts'
	end

    fun list m = elimDups m

    fun composemap (f: 'b -> 'c) (m: 'b map) : 'c map = 
	FM{elts=map (fn (a, b) => (a, f b)) (list m),unique=ref true}

    fun ComposeMap (f: dom * 'b -> 'c) (m: 'b map) : 'c map =
        FM{elts=map (fn (a, b) => (a, f(a, b))) (list m),unique=ref true}

    fun fold (f : ('a * 'b) -> 'b) (x : 'b) (m : 'a map) : 'b = 
	foldl (fn ((a, b), c) => f(b, c)) x (list m)

    fun Fold (f : ((dom * 'b) * 'c) -> 'c) (x : 'c) (m : 'b map) : 'c =
	foldl (fn ((a, b), c) => f((a, b), c)) x (list m)

    fun filter pred (m: 'b map) = 
	let val elts = list m
	in
	    FM{elts=List.filter pred elts,unique=ref true}
	end

    exception Restrict
    fun restrict(m: 'b map, dom : dom list) : 'b map =
      foldl(fn (d, acc) => 
		 case lookup m d
		   of SOME res => add(d,res,acc)
		    | NONE => raise Restrict) empty dom 

    fun enrich en (m0, m) =
      Fold(fn ((d,r),b) => b andalso
	   case lookup m0 d
	     of SOME r0 => en(r0,r)
	      | NONE => false) true m

    type Report = Report.Report

    fun reportMap f m = Report.flatten(map f (list m))

    type StringTree = PP.StringTree

    fun layoutMap {start, eq, sep, finish} layoutDom layoutRan m =
      let
	fun doit(x, y) = PP.NODE{start="", finish="", indent=0,
				 childsep=PP.RIGHT eq,
				 children=[layoutDom x, layoutRan y]
				}
      in
	PP.NODE{start=start, finish=finish, indent=0,
		childsep=PP.RIGHT sep, children=map doit (list m)
	       }
      end
  end;


