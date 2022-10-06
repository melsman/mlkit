(* Finite maps based on Patricia Trees. Source code adapted from
 * Okasaki & Gill, "Fast Mergeable Integer Maps", ML Workshop '98.
 * ME 1998-10-21.
 * NH 1998-12-08. Based on IntFinMapPT.sml
 *)

structure WordFinMap: MONO_FINMAP =
struct
  structure PP = PrettyPrint
  type dom = word

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

  fun singleton (d,r) = Lf(d, r)

  fun isEmpty Empty = true
    | isEmpty _ = false

  fun lookup t k =
    let val w = k
        fun look Empty = NONE
          | look (Lf (j,x)) = if j=w then SOME x else NONE
          | look (Br (p,m,t0,t1)) =
              if w <= p then look t0
                        else look t1
    in  look t end

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

  fun add (k,x,t) = ( insertw #1 (k,x,t))   (* was #2 *)

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
    | Fold f b (Lf(w,e)) = f((w,e),b)
    | Fold f b (Br(_,_,t1,t2)) = Fold f (Fold f b t1) t2

  fun remove (d,t) =  (* not terribly efficient! *)
    case ( lookup t d)
      of SOME _ => SOME(Fold (fn ((d',e),a) => if d=d' then a
					       else add(d',e,a)) empty t)
       | NONE => NONE

  fun composemap f Empty = Empty
    | composemap f (Lf(w,e)) = Lf(w,f e)
    | composemap f (Br(q1,q2,t1,t2)) = Br(q1,q2,composemap f t1, composemap f t2)

  fun ComposeMap f Empty = Empty
    | ComposeMap f (Lf(w,e)) = Lf(w,f(w,e))
    | ComposeMap f (Br(q1,q2,t1,t2)) = Br(q1,q2,ComposeMap f t1, ComposeMap f t2)

  fun app f Empty = ()
    | app f (Lf(w,e)) = f e
    | app f (Br(_,_,t1,t2)) = (app f t1; app f t2)

  fun dom t =
    let fun d (Empty, a) = a
	  | d (Lf(w,e), a) = w :: a
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

   (* Picklers *)

   local
     fun toInt Empty = 0
       | toInt (Lf _) = 1
       | toInt (Br _) = 2

     fun pu_Lf pu_a _ =
	 Pickle.con1 Lf (fn Lf a => a | _ => raise Fail "WordFinMapPT.pu_Lf")
	             (Pickle.pairGen0(Pickle.word,pu_a))

     fun pu_Br pu =
	 Pickle.con1 Br (fn Br a => a | _ => raise Fail "WordFinMapPT.pu_Br")
	             (Pickle.tup4Gen0(Pickle.word,Pickle.word,pu,pu))
   in
     fun pu _ (pu_a: 'a Pickle.pu) : 'a map Pickle.pu =
         Pickle.dataGen ("WordFinMapPT.map",toInt,[Pickle.con0 Empty, pu_Lf pu_a, pu_Br])

     fun puNoShare _ (pu_a: 'a Pickle.pu) : 'a map Pickle.pu =
         Pickle.dataGenNoShare ("WordFinMapPT.map",toInt,[Pickle.con0 Empty, pu_Lf pu_a, pu_Br])
   end

end


structure TestWordFinMap : sig end =
  struct
(*
    structure BasicIO = BasicIO()
    structure Report = Report(structure BasicIO=BasicIO)
    structure Crash=Crash(structure BasicIO = BasicIO)
    structure PP = PrettyPrint(structure Report=Report
			       structure Crash=Crash
			       structure Flags=Flags(structure Crash=Crash
						     structure Report=Report))

    structure IFM = IntFinMap(structure Report=Report
			      structure PP=PP)

    fun member [] e = false
      | member (x::xs) e = x=e orelse member xs e

    infix ===
    fun l1 === l2 =
      foldl (fn (x,b) => b andalso member l2 x) (length l1 = length l2) l1

    fun mk [] = []
      | mk (x::xs) = (x,Int.toString x)::mk xs

    fun mk' [] = []
      | mk' (x::xs) = (x,Int.toString x ^ "'")::mk' xs

    val l1 = mk [12,234,345,23,234,6,456,78,345,23,78,79,657,345,234,456,78,0,7,45,3,56,578,7,567,345,35,2,456,57,8,5]
    val l2 = mk' [23,43,4,456,456,23,4523,4,47,5,567,4356,345,34,79,78,53,5,5,6,47,567,56,7,46,345,34,5,36,47,57]

    val m1 = IFM.fromList l1
    val m2 = IFM.fromList l2

    val m3 = IFM.plus(m1,m2)

    fun test s true = print ("OK : " ^ s ^ "\n")
      | test s false = print ("ERROR : " ^ s ^ "\n")

    val test1 = test "test1" (IFM.list(m3) === IFM.list(IFM.fromList(l1@l2)))

    val test2 = test "test2" (IFM.lookup m1 6 = SOME "6")
    val test3 = test "test3" (IFM.lookup m1 9 = NONE)

    val test4 = test "test4" (IFM.lookup m3 4356 = SOME "4356'")

    val test5 = test "test5" (IFM.lookup m3 35 = SOME "35")

    val m4 = IFM.restrict (m3, [6,345,23,34,657,47])

    val test6 = test "test6" (IFM.lookup m4 23 = SOME "23'")
    val test6 = test "test6" (IFM.lookup m4 657 = SOME "657")
    val test7 = test "test7" (IFM.lookup m4 35 = NONE)
    val test8 = test "test8" (IFM.lookup m4 78 = NONE)

    val test9 = test "test9" ((IFM.restrict (m1,[43]); false) handle IFM.Restrict => true)

    fun sum [] = 0
      | sum (x::xs) = x + sum xs

    fun remdubs ([],a:int list) = a
      | remdubs (x::xs,a) = remdubs(xs, if member a x then a else x::a)

    val test10 = test "test10" (sum (IFM.dom m1) = sum (remdubs (map #1 l1,[])))

    val test11 = test "test11" (IFM.lookup (IFM.add(2222,"2222''",m1)) 2222 = SOME "2222''")
    val test12 = test "test12" (IFM.lookup (IFM.add(234,"234''",m1)) 234 = SOME "234''")

    val st = IFM.layoutMap {start="{",finish="}", eq=" -> ", sep=", "} (PP.LEAF o Int.toString) PP.LEAF m3

    val _ = PP.outputTree(print, st, 100)
*)
  end
