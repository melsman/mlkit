(* Finite maps using balanced AVL trees *)

functor OrderFinMap(structure Order : ORDERING
		    structure PP : PRETTYPRINT
		    structure Report : REPORT) : MONO_FINMAP =
  struct

    infix eq

    type dom = Order.T

    fun a < b = Order.lt a b
    val lt = op <

    fun (i1:dom) eq (i2:dom) = 
      not (i1 < i2 orelse i2 < i1)

    (* The balance of a tree is 'L', if the left subtree is one
       deeper than the right subtree, 'B' if the left and right subtrees
       have the same depth, and 'R' if the right subtree is one deeper than
       the left subtree: *)

    datatype bal = L | B | R 
    datatype 'b map = E | N of dom * 'b * 'b map * 'b map * bal

    val empty = E

    fun singleton (key, i) = N(key, i, E, E, B)

    fun isEmpty E = true
      | isEmpty _ = false

    fun lookup t key =
      let 
	fun search E = NONE
            | search(N(key', data, l, r, _)) = 
              if key < key' then search l
              else if key' < key then search r
              else (*key eq key' *) SOME data
      in 
	search t 
      end

    exception Impossible of string
    fun impossible s = raise (Impossible ("OrderFinMap" ^ s))

    local
      exception ALREADYTHERE
      fun insert((k0, d0), t) =
	let
	  fun ins E = (true, N(k0, d0, E, E, B))
	    | ins (N(k, d, l, r, bal)) = 
              if k0 < k then
                let 
		  val (higher, l') = ins l
                in 
		  case(bal,higher) of
		    (B,true)  => (true,  N(k, d, l', r, L))
		  | (B,false) => (false, N(k, d, l', r,  B))
		  | (R,true)  => (false, N(k, d, l', r,  B))
		  | (R,false) => (false, N(k, d, l', r,  R))
		  | (L,false) => (false, N(k, d, l', r, L))
		  | (L,true) =>
		      let 
			val (lk,ld,ll,lr,lbal) = 
			  case l' of
			    N d => d
			  | _ => impossible "AVLfinmap 1"  
		      in
			if lbal = L then 
			  (false, N(lk,ld,ll,N(k,d,lr,r,B),B))
			else (* lbal = R *)
			  let 
			    val (lrk,lrd,lrl,lrr,lrbal) = 
			      case lr of
				N d => d
			      | _ => impossible "AVLfinmap 2"  
			  in
			    (false, 
			     N(lrk,lrd,
			       N(lk,ld,ll,lrl, if lrbal=R then L else B),
			       N(k,d,lrr,r, if lrbal= L then R else B),
			       B))
			  end
		      end
		end
	      else if k < k0 (* k0 succeeds k *) then
		let 
		  val (higher, r') = ins r
                in 
		  case (bal,higher) of
		    (B,true) => (true, N(k,d,l,r',R))
		  | (B,false) => (false, N(k,d,l,r',B))
		  | (L,true) => (false,N(k,d,l,r',B))
		  | (L,false) => (false, N(k,d,l,r',L))
		  | (R, false) => (false,N(k,d,l,r',R))
		  | (R, true) => 
		      let 
			val (rk,rd,rl,rr,rbal) = 
			  case r' of
			    N d => d
			  | _ => impossible "AVLfinmap 3"  			    
		      in
			if rbal = R then 
			  (false, N(rk,rd,N(k,d,l,rl,B),rr,B))
			else (* rbal = L *)
			  let 
			    val (rlk,rld,rll,rlr,rlbal) = 
			      case rl of
				N d => d
			      | _ => impossible "AVLfinmap 4"  				
			  in
			    (false, 
			     N(rlk, rld,
			       N(k,d,l,rll, if rlbal=R then L else B),
			       N(rk,rd,rlr,rr,if rlbal = L then R else B),
			       B))
			  end
		      end
		end
              else (* k = k0 *) raise ALREADYTHERE
	in
	  #2(ins t)
	end 

      fun update((k0, d0), t) =
        let fun repl E = impossible "AVLupdate.repl"
              | repl (N(k,d,l,r,b)) =
                  if k0 < k then N(k,d,repl l, r, b)
                  else if k < k0 then N(k,d,l,repl r, b)
                  else (* k = k0 *) N(k,d0,l,r,b)
        in
            repl t
        end

    in
      fun add (k0:dom, d0:'b, t:'b map) : 'b map =
	insert((k0, d0), t) 
	handle ALREADYTHERE => update ((k0, d0), t)
    end

    fun plus (t1:'b map, t2:'b map) : 'b map =
      case t2 of
	E => t1
      | N(k,d,l,r,_) => 
	  plus (plus (add(k,d,t1), l), r) 
  
    local
      exception NOTFOUND
      fun delete(k0, t) =
	let
	  fun balance1 E = impossible "(balance1 on an empty map)"
	    | balance1 (N(k,d,l,r,bal)) =
              (* left branch has become lower *)
              case bal of 
                L => (N(k,d,l,r,B), true)
              | B => (N(k,d,l,r,R), false)
              | R => (* rebalance *)
		  let 
		    val (rk,rd,rl,rr,rbal) = 
		      case r of
			N d => d
		      | _ => impossible "AVLfinmap 5" 
		  in 
		    case rbal of 
		      L => let 
			     val (rlk,rld,rll,rlr,rlbal) = 
			       case rl of
				 N d => d
			       | _ => impossible "AVLfinmap 6" 
			     val bal' = if rlbal = R then L else B
			     val rbal' = if rlbal = L then R else B
			   in 
			     (N(rlk,rld,
				N(k,d,l,rll,bal'),
				N(rk,rd,rlr,rr,rbal'),
				B),
			      true)
			   end
		    | B => (N(rk,rd,
			      N(k,d,l,rl,R),
			      rr,L),false) 
		    | R => (N(rk,rd,
			      N(k,d,l,rl,B),
			      rr,B),true)
		  end
	  fun balance2 E = impossible "(balance2 on an empty map)"
	    | balance2 (N(k,d,l,r,bal)) =
              (* right branch has become lower *)
	      case bal of 
		R => (N(k,d,l,r,B), true)
	      | B => (N(k,d,l,r,L), false)
              | L => (* rebalance *)
		  let 
		    val (lk,ld,ll,lr,lbal) = 
		      case l of
			N d => d
		      | _ => impossible "AVLfinmap 7" 
		  in 
		    case lbal of 
		      R => let 
			     val (lrk,lrd,lrl,lrr,lrbal) = 
			       case lr of
				 N d => d
			       | _ => impossible "AVLfinmap 8" 				 
			     val bal' = if lrbal = L then R else B
			     val lbal' =if lrbal = R then L else B
			   in 
			     (N(lrk,lrd,
				N(lk,ld,ll, lrl,lbal'),
				N(k,d,lrr,r,bal'),
				B),
			      true)
			   end
		    | B => (N(lk,ld,
			      ll,
			      N(k,d,lr,r,L),
			      R),false) 
		    | L => (N(lk,ld,
			      ll,
			      N(k,d,lr,r,B),
			      B),true)
		  end

	  fun remove_rightmost E : 'b map * dom * 'b * bool  = 
	          impossible "(remove_rightmost on empty map)"
	    | remove_rightmost (N(k,d,l,E,bal)) = (l, k, d, true)
	    | remove_rightmost (N(k,d,l,r,bal)) = 
	      let 
		val (r',k',d',lower) = remove_rightmost r
              in 
		if lower then 
		  let 
		    val (t'', lower'') = balance2(N(k,d,l,r',bal))
		  in 
		    (t'',k',d',lower'')
		  end
		else 
		  (N(k,d,l,r',bal),k',d',false)
	      end

	  fun del E = raise NOTFOUND
	    | del (N(k,d,l,r,bal)) = 
              if k0 < k then 
                let 
		  val (l', lower) = del l 
		in 
		  if lower then balance1(N(k,d,l',r,bal))
		  else (N(k,d,l',r,bal), false)
                end
              else if k <  k0 then
                let 
		  val (r', lower) = del r
		in 
		  if lower then balance2(N(k,d,l,r',bal))
		  else (N(k,d,l,r',bal), false)
                end
              else (* k = k0 *)
                case (l,r) of
                  (_, E) => (l, true)
                | (E, _) => (r, true)
                |   _    => 
		      let 
			val (l', k', d', lower) = 
			  remove_rightmost l
		      in 
			if lower then balance1(N(k',d',l',r,bal))
			else (N(k',d',l',r,bal), false)
		      end
	in
	  #1(del t)
	end
    in
      fun remove(k:dom, t:'b map) : 'b map option =
	SOME(delete(k, t)) 
	handle NOTFOUND => NONE

      val delete : dom * 'b map -> 'b map = fn (k, t) =>
	delete(k, t) handle NOTFOUND => t 
    end

    fun dom (m:'b map) : (dom list) =
      let
	fun dom' E a = a
	  | dom' (N(k,_,l,r,_)) a =
	    dom' l (k::dom' r a)
      in
	dom' m []
      end
	
    fun range (m:'b map) : 'b list =
      let
	fun ran E a = a
	  | ran (N(_,d,l,r,_)) a =
	    ran l (d::ran r a)
      in
	ran m []
      end

    fun list (m:'b map) : (dom * 'b) list =
      let
	fun li E a = a
	  | li (N(k,d,l,r,_)) a =
	    li l ((k,d)::li r a)
      in
	li m []
      end

    fun composemap (f:'b -> 'c) (t:'b map) : 'c map =
      case t of
	E => E
      | N(k,d,l,r,b) => 
	  let
	    val l' = composemap f l
	  in 
	    N(k,f d,l',composemap f r,b)
	  end

    fun ComposeMap (f: dom * 'b -> 'c) (t:'b map) : 'c map =
      case t of
	E => E
      | N(k,d,l,r,b) => 
	  let
	    val l' = ComposeMap f l
	  in 
	    N(k,f (k,d),l',ComposeMap f r, b)
	  end

    fun fold (f:'a * 'b -> 'b) (e : 'b) (t : 'a map) : 'b =
      let
	fun fold' E a = a
	  | fold' (N(_,d,l,r,_)) a =
	    fold' r (f (d, fold' l a))
      in
	fold' t e
      end

    fun Fold (f:(dom * 'a) * 'b -> 'b) (e : 'b) (t : 'a map) : 'b =
      let
	fun fold' E a = a
	  | fold' (N(k,d,l,r,_)) a =
	    fold' r (f ((k,d), fold' l a))
      in
	fold' t e
      end

    fun filter (f:dom * 'b -> bool) (t:'b map) =
      Fold (fn ((k,d), t') => if f (k,d) then add(k,d,t')
			      else t') empty t

    fun addList [] (t : 'b map) : 'b map = t
      | addList ((k : dom, d : 'b) :: rest) t = 
	   addList rest (add (k, d, t)) 

    fun fromList l = addList l empty

    (* constructing an AVL-tree from a sorted list in 
       linear time.  Mikkel Thorup and Mads Tofte    *)

    fun fromSortedList l =
    let
       fun power2 0 = false
         | power2 1 = true
	 | power2 n = n mod 2 = 0 andalso power2 (n div 2)
         
       datatype ('a,'b) tree = 
	   Lf 
	 | Node of 'a * 'b * ('a,'b) tree * ('a,'b) tree

       fun build(0,xs) = (xs, Lf)
	 | build(n,xs) = 
	   let val n' = n div 2
	     val (xs1, t1) = build(n', xs)
	     val (d,r,xs1') = case xs1
				of (d,r) :: xs1' => (d,r,xs1')
				 | _ => impossible "build" 
(*old
	     val (d,r) :: xs1' = xs1
old*)
	     val (xs2, t2) = build (n -n' -1, xs1')
	   in
	     (xs2, Node(d,r,t1,t2))
	   end

       fun max(i:int, j) = if i>j then i else j

       fun mk_avl(Lf) = (E, 0)
         | mk_avl(Node(d,r,t1,t2)) =
             let val (t1', depth1) = mk_avl t1
                 val (t2', depth2) = mk_avl t2
             in (N(d,r,t1',t2', if depth1=depth2 then B else if depth1 > depth2 then L else R),
                 1 + max(depth1,depth2))
             end

    in
       #1(mk_avl(#2(build(length l, l))))
    end

(* old
    fun mergeMap (f:'b * 'b -> 'b) (t1:'b map) (t2:'b map) : 'b map =
      let
	fun merge E t2 a = plus (t2,a)
	  | merge t1 E a = plus (t1,a)
	  | merge (t1 as N(k,d,l,r,_)) t2 a =
	    let
	      val (e, t2') = 
		case lookup t2 k of
		  SOME d' => (f (d,d'), delete (k, t2))
		| NONE => (d, t2)
	      val t1' = delete (k, t1)
	    in
	      merge t1' t2' (add(k,e,a))
	    end
      in
	merge t1 t2 empty
      end
old *)
	  
    (* mergeMap f t1 t2   merges t1 and t2 into a single map
       which has domain  dom(t1) union dom(t2) and which
       satifies  (mergeMap f t1 t2) x = f(t1(x), t2(x)), for
       all x in dom t1 intersect dom t2.

       The construction takes linear time in the sum of the sizes
       of t1 and t2.
    *)

    fun mergeMap (f:'b * 'b -> 'b) (t1:'b map) (t2:'b map) : 'b map =
      let
        (* merge lists *)
	fun merge ([], ys) = ys
	  | merge (xs, []) = xs
	  | merge (l as((d1,r1)::xs), l' as ((d2,r2)::ys)) = 
	    if lt(d1,d2) then (d1,r1):: merge(xs, l')
	    else if lt(d2,d1) then (d2,r2)::merge(l, ys)
		 else (* d1=d2 *)
		   (d1, f(r1,r2))::merge(xs,ys)

      in
         fromSortedList(merge(list t1, list t2))
      end

    fun oneForWhich (f : ((dom * 'b) -> bool)) (t : 'b map) 
          : (dom * 'b) option =
      case t of
	E => NONE
      | N(k,d,l,r,_) => 
	  if f (k,d) then SOME (k,d)
	  else 
	    case oneForWhich f l of
	      SOME p => SOME p
	    | NONE => oneForWhich f r  


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

    type StringTree = PP.StringTree

    fun layoutMap {start, eq=equal, sep, finish} 
      layoutDom layoutRan m =
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

    val reportMapSORTED  = reportMap

  end
