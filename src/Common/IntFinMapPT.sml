(* Finite maps based on Patricia Trees. Source code adapted from
 * Okasaki & Gill, "Fast Mergeable Integer Maps", ML Workshop '98.
 * ME 1998-10-21.
 *)

structure IntFinMap: MONO_FINMAP =
struct
  
  structure PP = PrettyPrint
  type dom = int
  
  (* helper functions *)
  open Word
  fun lowestBit x = andb (x,0w0 - x)
  fun max (x,y) = if x>y then x else y
  fun highestBit (x,m) =
    let val x' = andb (x,notb (m-0w1))
        fun highb (x,m) =
          if x=m then m else highb (andb (x,notb m),m+m)
    in highb (x',m) end
  fun branchingBit (m,p0,p1) = highestBit (xorb (p0,p1), m)
  fun mask (k,m) = orb (k,m-0w1+m) - m
  fun zeroBit (k,m) = (andb (k,m) = 0w0)
  fun matchPrefix (k,p,m) = (mask (k,m) = p)
  fun swap (x,y) = (y,x)

  datatype 'a map =
      Empty
    | Lf of word * 'a
    | Br of word * word * 'a map * 'a map
  (* 
   * Lf (k,x): 
   *   k is the key 
   * Br (p,m,t0,t1):
   *   p is the largest common prefix for all the keys in this tree
   *   m is the branching bit
   *     (m is a power of 2, only the bits above m are valid in p)
   *   t0 contains all the keys with a 0 in the branching bit
   *   t1 contains all the keys with a 1 in the branching bit
   *)

  val empty = Empty

  fun check s d = if Int.<(d, 0) then print ("IntFinMapPT." ^ s ^ " error\n")  
		  else ()

  fun singleton (d,r) = 
    ((* check "singleton" d; *)
     Lf (fromInt d, r))

  fun isEmpty Empty = true
    | isEmpty _ = false

  fun lookupw t w =
    let fun look Empty = NONE
          | look (Lf (j,x)) = if j=w then SOME x else NONE
          | look (Br (p,m,t0,t1)) =
              if w <= p then look t0
                        else look t1
    in look t end

  fun lookup t k = lookupw t (fromInt k)

  fun join (m,p0,t0,p1,t1) =
    (* combine two trees with prefixes p0 and p1,
     * where p0 and p1 are known to disagree
     *)
    let val m = branchingBit (m,p0,p1)
    in if p0 < p1 then Br (mask (p0,m), m, t0, t1)
                  else Br (mask (p0,m), m, t1, t0)
    end

  fun insertw c (w,x,t) =
    let fun ins Empty = Lf (w,x)
          | ins (t as Lf (j,y)) =
              if j=w then Lf (w,c (x,y))
              else join (0w1,w,Lf (w,x),j,t)
          | ins (t as Br (p,m,t0,t1)) =
              if matchPrefix (w,p,m) then
                if w <= p then Br (p,m,ins t0,t1)
                          else Br (p,m,t0,ins t1)
              else join (m+m,w,Lf (w,x),p,t)
    in ins t end

  fun add (k,x,t) = ((*check "add" k;*) insertw #1 (fromInt k,x,t))   (* was #2 *)

  fun merge c (s,t) =
    let fun mrg (s as Br (p,m,s0,s1), t as Br (q,n,t0,t1)) =
              if m<n then
                if matchPrefix (p,q,n) then
                  if p <= q then Br (q,n,mrg (s,t0),t1)
                           else Br (q,n,t0,mrg (s,t1))
                else join (n+n,p,s,q,t)
              else if m>n then
                if matchPrefix (q,p,m) then
                  if q <= p then Br (p,m,mrg (s0,t),s1)
                           else Br (p,m,s0,mrg (s1,t))
                else join (m+m,p,s,q,t)
              else (* if m=n then *)
                if p=q then Br (p,m,mrg (s0,t0),mrg (s1,t1))
                else join (m+m,p,s,q,t)
          | mrg (t as Br _, Lf (w,x)) = insertw (c o swap) (w,x,t)
          | mrg (t as Br _, Empty) = t
          | mrg (Lf (w,x), t) = insertw c (w,x,t)
          | mrg (Empty, t) = t
    in mrg (s,t)
    end

  fun plus (s,t) = merge #2 (s,t)

  fun mergeMap c s t = merge c (s,t)

  fun fold f b Empty = b
    | fold f b (Lf(w,e)) = f(e,b)
    | fold f b (Br(_,_,t1,t2)) = fold f (fold f b t1) t2

  fun Fold f b Empty = b
    | Fold f b (Lf(w,e)) = f((toInt w,e),b)
    | Fold f b (Br(_,_,t1,t2)) = Fold f (Fold f b t1) t2

(*
  fun remove (k,t) =  (* not terribly efficient! *)
    case ((* check "remove" k;*) lookup t k)
      of SOME _ => SOME(Fold (fn ((k',e),a) => if k=k' then a
					       else add(k',e,a)) empty t)
       | NONE => NONE
*)

  fun removew (w,t) =
      case t of                
        Empty => NONE
      | Lf(w',e) => if w' = w then SOME Empty
                    else NONE
      | Br(p,m,t0,t1) =>
        if w <= p then
          case removew(w,t0) of
            NONE => NONE
          | SOME t0' => SOME (plus(t0',t1))
        else
          case removew(w,t1) of
            NONE => NONE
          | SOME t1' => SOME (plus(t0,t1'))

  fun remove (k,t) = removew (fromInt k,t)      

  fun composemap f Empty = Empty
    | composemap f (Lf(w,e)) = Lf(w,f e)
    | composemap f (Br(q1,q2,t1,t2)) = Br(q1,q2,composemap f t1, composemap f t2)

  fun ComposeMap f Empty = Empty
    | ComposeMap f (Lf(w,e)) = Lf(w,f(toInt w,e))
    | ComposeMap f (Br(q1,q2,t1,t2)) = Br(q1,q2,ComposeMap f t1, ComposeMap f t2)

  fun app f Empty = ()
    | app f (Lf(w,e)) = f e
    | app f (Br(_,_,t1,t2)) = (app f t1; app f t2)

  fun dom t =
    let fun d (Empty, a) = a
	  | d (Lf(w,e), a) = toInt w :: a
	  | d (Br(_,_,t1,t2), a) = d(t2,d(t1,a))
    in d(t,[])
    end

  fun range m = fold (op ::) nil m
  fun list m = Fold (op ::) nil m
  fun filter f m = Fold (fn (e as (d,r),a) => if f e then add(d,r,a) 
					      else a) empty m 
  fun addList [] m = m
    | addList ((d,r)::rest) m = addList rest (add(d,r,m))

  fun fromList l = addList l empty
    
  exception Restrict of string
  fun restrict (pp, m, l) =
    let fun res ([], a) = a
	  | res (d::rest, a) =
      case lookup m d
	       of SOME r => res(rest,add(d,r,a))
		| NONE => raise Restrict(pp d)
    in res (l, empty)
    end
(*
   fun enrich f (m1, m2) =
     Fold (fn ((d2,r2),b) => 
	   case lookup m1 d2
	     of SOME r1 => b andalso f(r1,r2)
	      | NONE => false) true m2  
*)
   fun enrich f p =
       let fun enr (_, Empty) = true
             | enr (Empty, _) = false                           
             | enr (Lf _, Br _) = false
             | enr (t1, Lf(d2,r2)) =
               (case lookupw t1 d2 of
                  SOME r1 => f(r1,r2)
                | NONE => false)
             | enr (t1 as Br(p1,_,t11,t12), t2 as Br(p2,_,t21,t22)) =
               if p1 > p2 then enr (t11,t21) andalso enr (t1,t22)
               else if p1 < p2 then enr (t12, t22) andalso enr (t1, t21)
               else enr (t11,t21) andalso enr (t12,t22)
       in enr p
       end

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


   (* Pickler *)

   fun pu _ (pu_a: 'a Pickle.pu) : 'a map Pickle.pu =
       let fun toInt Empty = 0
	     | toInt (Lf _) = 1
	     | toInt (Br _) = 2

	   val pu_Empty = Pickle.con0 Empty

	   fun pu_Lf _ =
	       Pickle.con1 Lf (fn Lf a => a | _ => raise Fail "IntFinMapPT.pu_Lf")
	       (Pickle.pairGen0(Pickle.word,pu_a))

	   fun pu_Br pu =
	       Pickle.con1 Br (fn Br a => a | _ => raise Fail "IntFinMapPT.pu_Br")
	       (Pickle.tup4Gen0(Pickle.word,Pickle.word,pu,pu))
       in
	   Pickle.dataGen ("IntFinMapPT.map",toInt,[pu_Empty, pu_Lf, pu_Br])
       end
end
