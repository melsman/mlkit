(* Finite sets using balanced AVL trees *)

functor IntSet(structure PP : PRETTYPRINT): KIT_MONO_SET =
  struct

    infix ==

    type elt = int

    fun lt ((a:elt),b) = a<b

    fun (i1:elt) == (i2:elt) : bool = i1 = i2

    exception Impossible of string
    fun impossible s = raise (Impossible s)

    (* The balance of a tree is 'L', if the left subtree is one
       deeper than the right subtree, 'B' if the left and right subtrees
       have the same depth, and 'R' if the right subtree is one deeper than
       the left subtree: *)

    datatype bal = L | B | R 
    datatype Set = E | N of elt * Set * Set * bal

    val empty = E

    fun singleton i = N(i, E, E, B)

    fun size E = 0
      | size (N(_, s1, s2, _)) = 1 + size s1 + size s2

    fun isEmpty E = true
      | isEmpty _ = false

    fun member (i:elt) (s:Set) : bool =
      let 
	fun search E = false
	  | search(N(i', l, r, _)) = 
	      if i == i' then true
	      else if i < i' then search l
		   else search r
      in 
	search s 
      end

    fun eq s1 s2 = 
      size s1 = size s2 andalso
      (* each member of s1 must be a member of s2 *)
      let
	fun eq' E = true
	  | eq' (N(i, l, r, _)) = 
	    member i s2 andalso eq' l andalso eq' r
      in
	eq' s1
      end

    exception ALREADYTHERE
    fun insert k0 t =
      let
	fun ins E = (true, N(k0, E, E, B))
	  | ins (N(k, l, r, bal)) = 
	    if k0 == k then raise ALREADYTHERE
	    else if k0 < k then
	      let 
		val (higher, l') = ins l
	      in 
		case(bal,higher) of
		  (B,true)  => (true, N(k, l', r, L))
		| (B,false) => (false, N(k, l', r, B))
		| (R,true)  => (false, N(k, l', r, B))
		| (R,false) => (false, N(k, l', r, R))
		| (L,false) => (false, N(k, l', r, L))
		| (L,true) =>
		    let 
		      val (lk,ll,lr,lbal) = 
			case l' of
			  N d => d
			| _ => impossible "empty tree01!"
		    in
		      if lbal = L then 
			(false, N(lk,ll,N(k,lr,r,B),B))
		      else (* lbal = R *)
			let 
			  val (lrk,lrl,lrr,lrbal) = 
			    case lr of
			      N d => d
			    | _ => impossible "empty tree02!"
			in
			  (false, 
			   N(lrk,
			     N(lk,ll,lrl, if lrbal=R then L else B),
			     N(k,lrr,r, if lrbal= L then R else B),
			     B))
			end
		    end
	      end
	    else (* k0 succeeds k *)
	      let 
		val (higher, r') = ins r
	      in 
		case (bal,higher) of
		  (B,true) => (true, N(k,l,r',R))
		| (B,false)=> (false, N(k,l,r',B))
		| (L,true)=> (false,N(k,l,r',B))
		| (L,false)=> (false, N(k,l,r',L))
		| (R, false)=> (false,N(k,l,r',R))
		| (R, true) => 
		    let 
		      val (rk,rl,rr,rbal) = 
			case r' of
			  N d => d
			| _ => impossible "empty tree03!"
		    in
		      if rbal = R then 
			(false, N(rk,N(k,l,rl,B),rr,B))
		      else (* rbal = L *)
			let 
			  val (rlk,rll,rlr,rlbal) = 
			    case rl of
			      N d => d
			    | _ => impossible "empty tree04!"
			in
			  (false, 
			   N(rlk,
			     N(k,l,rll, if rlbal=R then L else B),
			     N(rk,rlr,rr,if rlbal = L then R else B),
			     B))
			end
		    end
	      end
      in
	#2(ins t) handle ALREADYTHERE => t
      end  (* insert *)

    fun list (s:Set) : elt list =
      let
	fun f E a = a
	  | f (N(i, s1, s2, _)) a = 
	       f s1 (i::(f s2 a))
      in
	f s []
      end

    fun fromList (l:elt list) : Set =
      case l of
	[] => empty
      | (i::is) => insert i (fromList is)

    fun union (s1:Set) (s2:Set) : Set =
      case s2 of
	E => s1
      | N(i, s3, s4, _) => 
	  union (union (insert i s1) s3) s4

    fun addList [] s = s
      | addList (i::ls) s = addList ls (insert i s)

    exception NOTFOUND
    fun remove k0 t =
      let
	fun balance1 E = impossible "(balance1 on an empty tree)"
	  | balance1 (N(k,l,r,bal)) =
	    (* left branch has become lower *)
	    case bal of 
	      L => (N(k,l,r,B), true)
	    | B => (N(k,l,r,R), false)
	    | R => (* rebalance *)
		let 
		  val (rk,rl,rr,rbal) = 
		    case r of
		      N d => d
		    | _ => impossible "empty tree11!"
		in 
		  case rbal of 
		    L => let 
			   val (rlk,rll,rlr,rlbal) = 
			     case rl of
			       N d => d
			     | _ => impossible "empty tree12!"
			   val bal' = if rlbal = R then L else B
			   val rbal' =if rlbal = L then R else B
			 in 
			   (N(rlk,
			      N(k,l,rll,bal'),
			      N(rk,rlr,rr,rbal'),
			      B),
			    true)
			 end
		  | B => (N(rk,
			    N(k,l,rl,R),
			    rr,L),false) 
		  | R => (N(rk,
			    N(k,l,rl,B),
			    rr,B),true)
		end
	fun balance2 E = impossible "(balance2 on an empty tree)"
	  | balance2 (N(k,l,r,bal)) =
	    (* right branch has become lower *)
	    case bal of 
	      R => (N(k,l,r,B), true)
	    | B => (N(k,l,r,L), false)
	    | L => (* rebalance *)
		let 
		  val (lk,ll,lr,lbal) = 
		    case l of
		      N d => d
		    | _ => impossible "empty tree21!" 
		in 
		  case lbal of 
		    R => let 
			   val (lrk,lrl,lrr,lrbal) = 
			     case lr of
			       N d => d
			     | _ => impossible "empty tree22!"
			   val bal' = if lrbal = L then R else B
			   val lbal' =if lrbal = R then L else B
			 in 
			   (N(lrk,
			      N(lk,ll, lrl,lbal'),
			      N(k,lrr,r,bal'),
			      B),
			    true)
			 end
		  | B => (N(lk,
			    ll,
			    N(k,lr,r,L),
			    R),false) 
		  | L => (N(lk,
			    ll,
			    N(k,lr,r,B),
			    B),true)
		end
  
	fun remove_rightmost E : Set * elt * bool  = 
  	      impossible "(remove_rightmost on empty tree)"
	  | remove_rightmost (N(k,l,E,bal)) = (l, k, true)
	  | remove_rightmost (N(k,l,r,bal)) = 
	    let 
	      val (r',k',lower) = remove_rightmost r
	    in 
	      if lower then 
		let 
		  val (t'', lower'') = balance2(N(k,l,r',bal))
		in 
		  (t'',k',lower'')
		end
	      else 
		(N(k,l,r',bal),k',false)
	    end
	  
	fun del E = raise NOTFOUND
	  | del (N(k,l,r,bal)) = 
	    if k0 < k then 
	      let 
		val (l', lower) = del l 
	      in 
		if lower then balance1(N(k,l',r,bal))
		else (N(k,l',r,bal), false)
	      end
	    else if k < k0 then
	      let 
		val (r', lower) = del r
	      in 
		if lower then balance2(N(k,l,r',bal))
		else (N(k,l,r',bal), false)
	      end
	    else (* k = k0 *)
	      case (l,r) of
		(_, E) => (l, true)
	      | (E, _) => (r, true)
	      |   _    => 
		    let 
		      val (l', k', lower) = 
			remove_rightmost l
		    in 
		      if lower then balance1(N(k',l',r,bal))
		      else (N(k',l',r,bal), false)
		    end
      in
	#1(del t) handle NOTFOUND => t
      end

    (* difference : s1 \ s2 *)
    fun difference (s1:Set) (s2:Set) : Set =
      (* remove items in s2 from s1 *)
      case s1 of
	E => E
      | _ => (case s2 of
		E => s1
	      | N(i, l, r, _) => 
		  difference (difference (remove i s1) l) r)

    fun intersect (s1:Set) (s2:Set) : Set =
      (* Build up a new set from elements in s1 which 
         are also members of s2. *)
      let
	fun inters E a = a
	  | inters (N(i,r,l,_)) a =
	    inters r (inters l (if member i s2 then insert i a 
				else a))
      in
	inters s1 empty
      end

    fun partition (f:elt -> bool) (s:Set) : Set * Set =
      let
	fun g E p = p
	  | g (N(i,s1,s2,_)) (s3, s4) =
	    let
	      val b = if f i then (insert i s3, s4)
		      else (s3, insert i s4)
	    in 
	      g s2 (g s1 b)
	    end
      in
	g s (empty, empty)
      end

    fun fold (f: elt -> 'b -> 'b) (e:'b) (t:Set) : 'b =
      let
	fun fold' (E, a) = a
	  | fold' (N(i,E,E,_), a) = f i a
	  | fold' (N(i,s1,E,_), a) = f i (fold' (s1, a))
	  | fold' (N(i,E,s2,_), a) = fold' (s2, f i a)
	  | fold' (N(i,s1,s2,_), a) = fold' (s2, (f i (fold' (s1, a))))
      in
	fold' (t, e)
      end

    (* the usual list map *)
    val listmap = map

    fun map (f:elt -> elt) (t:Set) : Set =
      fromList (listmap f (list t))

    fun subst (i':elt, i:elt) (s:Set) : Set =
      if member i s then insert i' (remove i s) else s

    fun apply (f:elt -> unit) (s:Set) =
      let
	fun appl E = ()
	  | appl (N(i,l,r,_)) = (appl l; f i; appl r)
      in
	appl s
      end

    type StringTree = PP.StringTree

    fun layoutSet {start, sep, finish} layoutItem s =
      PP.NODE {start=start,
	       finish=finish,
	       indent=3,
	       childsep=PP.RIGHT sep,
	       children=listmap layoutItem (list s)}

    val pu_bal =
	let open Pickle
	in enumGen [L,B,R]
	end

    fun pu pu_elt = 
	let open Pickle
	    fun toInt E = 0
	      | toInt (N _) = 1
	    fun eq (E,E) = true
	      | eq (N (e1,s1,s1',b1), N(e2,s2,s2',b2)) =
		#4 pu_elt (e1,e2) andalso b1 = b2 andalso
		eq (s1,s2) andalso eq (s1',s2')
	      | eq _ = false		
	    fun funE (_ : Set pu) : Set pu = 
		(fn _ => fn spe => spe,
		 fn supe => (E,supe),
		 fn ds => fn _ => 0w0,
		 eq)
	    fun funN (pu : Set pu) : Set pu =
		(fn n => fn spe =>
		 case n of 
		     N(e,s1,s2,b) => let val spe = pickler pu_elt e spe
					 val spe = pickler pu s1 spe
					 val spe = pickler pu s2 spe
				     in pickler pu_bal b spe
				     end
		   | _ => raise Fail "OrderSet.pu.funN.pickle",
		 fn supe =>
		 let val (e,supe) = unpickler pu_elt supe
		     val (s1,supe) = unpickler pu supe
		     val (s2,supe) = unpickler pu supe
		     val (b,supe) = unpickler pu_bal supe
		 in (N(e,s1,s2,b),supe)
		 end,
		 fn N(e,s1,s2,b) => 
		 hashCombine(hasher pu_elt e,
			     hashCombine(hasher pu_bal b,
					 hashCombine(hasher pu s1,
						     hasher pu s2)))
		  | _ => raise Fail "OrderSet.pu.funN.hasher",
		 eq)					      
	in dataGen (toInt, eq, [funE,funN])
	end

  end

