(* Finite maps based on Patricia Trees. Source code adapted from
 * Okasaki & Gill, "Fast Mergeable Integer Maps", ML Workshop '98.
 * ME 1998-10-21.
 *)

functor IntFinMap(structure PP : PRETTYPRINT
		  structure Report : REPORT) : MONO_FINMAP =
struct
  
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
  fun max2 (m,n) = if m > n then m+m else n+n

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

  fun singleton (d,r) = Lf (fromInt d, r)

  fun isEmpty Empty = true
    | isEmpty _ = false

  fun lookup t k =
    let val w = fromInt k
        fun look Empty = NONE
          | look (Lf (j,x)) = if j=w then SOME x else NONE
          | look (Br (p,m,t0,t1)) =
              if w <= p then look t0
                        else look t1
    in look t end

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

  fun add (k,x,t) = insertw #2 (fromInt k,x,t)

  fun merge c (s,t) =
    let 
        fun mrg (s as Br (p,m,s0,s1), t as Br (q,n,t0,t1)) =
              if m>n then
                if matchPrefix (p,q,n) then
                  if p <= q then Br (q,n,mrg (s,t0),t1)
                           else Br (q,n,t0,mrg (s,t1))
                else join (max2 (m,n),p,s,q,t)
              else if m<n then
                if matchPrefix (q,p,m) then
                  if q <= p then Br (p,m,mrg (s0,t),s1)
                           else Br (p,m,s0,mrg (s1,t))
                else join (max2 (m,n),p,s,q,t)
              else (* if m=n then *)
                if p=q then Br (p,m,mrg (s0,t0),mrg (s1,t1))
                else join (max2 (m,n),p,s,q,t)
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

  fun fromList l =
    let fun fl ([], a) = a
	  | fl ((d,r)::rest, a) = fl(rest,add(d,r,a))
    in fl(l,empty)
    end

  fun remove (d,t) =  (* not terribly efficient! *)
    case lookup t d 
      of SOME _ => SOME(Fold (fn ((d',e),a) => if d=d' then a
					       else add(d',e,a)) empty t)
       | NONE => NONE

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
    
  exception Restrict
  fun restrict (m, l) =
    let fun res ([], a) = a
	  | res (d::rest, a) =
      case lookup m d
	       of SOME r => res(rest,add(d,r,a))
		| NONE => raise Restrict
    in res (l, empty)
    end

   fun enrich f (m1, m2) =
     Fold (fn ((d2,r2),b) => 
	   case lookup m1 d2
	     of SOME r1 => b andalso f(r1,r2)
	      | NONE => false) true m2  

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

end
