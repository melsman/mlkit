(* Finite maps *)

functor FinMap(structure Report: REPORT
	       structure PP: PRETTYPRINT
	      ): FINMAP =
  struct

    datatype (''a, 'b) map = FM of {elts: (''a * 'b) list, unique : bool ref}

    val reftrue = ref true
    val empty = FM {elts = [], unique = reftrue}    

    fun singleton p = FM {elts = [p], unique = reftrue}

    fun isEmpty (FM{elts=[],...}) = true
      | isEmpty _ = false

    fun lookup (FM{elts=l,...}) x = 
	let fun look [] = NONE
	      | look ((x',y)::rest) = if x=x' then SOME(y) else look rest
	in
	    look l
	end

    fun isin(x,FM{elts=l,...}) = 
	let fun look [] = false
	      | look ((x',_)::rest) = x=x' orelse look rest
	in
	    look l
	end

    fun add (x, y, FM{elts=l,...}) = FM{elts=(x,y)::l,unique=ref false}

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
		if x=x' then 
		    if is_unique then 
			SOME(FM{elts=revappend(a,rest),unique=reftrue})
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
	      if x = x' then (x, folder(y, y')) :: rest
	      else (x, y) :: insert(x', y', rest)
      in
	FM{elts=foldl (fn ((x, y), m) => insert(x, y, m)) map1 map2,
	   unique= (if !u1 andalso !u2 then reftrue
		    else ref false)}
      end

    fun elimDuplicates l = 
        let val duplicates = ref false
	    fun seen(x,[]) = false
	      | seen(x,(x',_)::rest) = x=x' orelse seen(x,rest)
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

    fun dom(FM{elts=m,unique}) = 
	let val d = map #1 m
	in
	    if !unique then (*EqSet.fromUniqueList*) EqSet.fromList d
	    else EqSet.fromList d
	end

    fun range m = 
	let val elts' = elimDups m
	in
	    map #2 elts'
	end

    fun list m = elimDups m

    fun composemap (f: 'b -> 'c) (m: (''a, 'b) map): (''a, 'c) map = 
	FM{elts=map (fn (a, b) => (a, f b)) (list m),unique=reftrue}

    fun ComposeMap (f: ''a * 'b -> 'c) (m: (''a, 'b) map): (''a, 'c) map =
        FM{elts=map (fn (a, b) => (a, f(a, b))) (list m),unique=reftrue}

    fun fold (f : ('a * 'b) -> 'b) (x : 'b) (m : (''d,'a) map) : 'b = 
	foldl (fn ((a, b), c) => f(b, c)) x (list m)

    fun Fold (f : ((''a * 'b) * 'c) -> 'c) (x : 'c) (m : (''a,'b) map) : 'c =
	foldl (fn ((a, b), c) => f((a, b), c)) x (list m)

    fun filter pred (m:(''a,'b) map) = 
	let val elts = list m
	in
	    FM{elts=List.filter pred elts,unique=reftrue}
	end

    fun addList [] t = t : (''a,'b) map
      | addList ((k : ''a, d : 'b) :: rest) t = 
	  addList rest (add (k, d, t))

    fun fromList l = addList l empty

    type Report = Report.Report

    fun reportMap f m = Report.flatten(map f (list m))

    fun reportMapSORTED lt f m =
      Report.flatten
      (map f (ListSort.sort (fn (a,_) => fn (b,_) => lt(a,b)) (list m)))

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

    fun uniquify fm = FM {elts=elimDups fm,unique=reftrue}

    fun pu (pu_d, pu_r) =
	let fun to (es,br) = FM {elts=es,unique=br}
	    fun from (FM {elts,unique}) = (elts, unique)
	in Pickle.convert (to,from o uniquify)
	    (Pickle.pairGen(Pickle.listGen(Pickle.pairGen(pu_d,pu_r)),
			    Pickle.refOneGen Pickle.bool))
	end
(*
    fun eq eq_e (m1,m2) =
	let fun eqs (nil,nil) = true
	      | eqs ((a1,e1)::es1,(a2,e2)::es2) = a1=a2 andalso eqs (es1,es2) andalso eq_e(e1,e2)
	      | eqs _ = false
	in eqs (list m1,list m2)
	end
*)
  end;


