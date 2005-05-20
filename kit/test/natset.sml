signature MONO_SET =
  sig
    type elt
    type Set

    val empty : Set
    val singleton : elt -> Set

    val size : Set -> int
    val isEmpty : Set -> bool
    val member : elt -> Set -> bool
    val eq : Set -> Set -> bool

    val list : Set -> elt list
    val fromList : elt list -> Set 
    val addList : elt list -> Set -> Set
      (* addList l s : Add elements in list l to s. *)

    val insert : elt -> Set -> Set
    val remove : elt -> Set -> Set
    val difference : Set -> Set -> Set
    val intersect : Set -> Set -> Set
    val union : Set -> Set -> Set
    val partition : (elt -> bool) -> Set -> Set * Set

    val subst : elt * elt -> Set -> Set
      (* subst (a,b) s : Substitute element b in s with element a. *)

    val fold : (elt -> 'b -> 'b) -> 'b -> Set -> 'b
      (* fold f base s; folds using f over the base element. *)

    val map : (elt -> elt) -> Set -> Set
      (* map f s; builds a new set by applying f to each element in s *)

    val apply : (elt -> unit) -> Set -> unit
      (* apply f s; applies f to each element of s (in order) *)
  end

structure NatSet : MONO_SET =
  struct 
    structure Bits =
      struct
	val xorb : word * word -> word = Word.xorb
	val lshift : word * word -> word = Word.<<
	val andb : word * word -> word = Word.andb
	val notb : word -> word = Word.notb
	val rshift : word * word -> word = Word.>>
	val orb : word * word -> word = Word.orb
      end

    val bits_word = 0w31
    fun bit n = Bits.lshift(0w1,n)
    fun setb(w,n) = Bits.orb(w,bit n)
    fun isb(w,n) = Bits.andb(Bits.rshift(w,n),0w1) <> 0w0
    fun unsetb(w,n) = Bits.andb(w, Bits.notb (bit n))
      
    datatype natset = 
        empty 
      | some of word * natset * natset

    type Set = natset
    type elt = word

    fun member (n, empty) = false
      | member (n, some(w,t1,t2)) =
         if n < bits_word then isb(w,n)
	 else if Bits.andb(n,0w1) <> 0w0 then member (Bits.rshift(n-bits_word,0w1), t1)
         else member(Bits.rshift(n-bits_word-0w1,0w1), t2)

    fun add0(empty,n) = 
         if n < bits_word then some(setb(0w0,n),empty,empty)
	 else if Bits.andb(n,0w1) <> 0w0 then some(0w0,add0(empty, Bits.rshift(n-bits_word,0w1)), empty)
	 else some(0w0, empty, add0(empty, Bits.rshift(n-bits_word-0w1,0w1)))
      | add0(some(w,t1,t2),n) =
         if n < bits_word then some(setb(w,n),t1,t2)
	 else if Bits.andb(n,0w1) <> 0w0 then some(w,add0(t1, Bits.rshift(n-bits_word,0w1)), t2)
	 else some(w, t1, add0(t2, Bits.rshift(n-bits_word-0w1,0w1)))

    fun singleton i = add0(empty,i)
      
    fun add(is,n) = if member(n,is) then is else add0(is,n)
	
    fun count(w,n) = if n < bits_word then 
                       if isb(w,n) then 1 + count(w,n+0w1)
		       else count(w,n+0w1)
		     else 0

    fun cardinality is =
      let fun count(0w0,n,a) = a
	    | count(w,n,a) = if isb(w,n) then count(unsetb(w,n),n+0w1,a+1)
			     else count(w,n+0w1,a)
	  fun c (empty,a) = a
	    | c (some(w,t1,t2),a) = c(t2,c(t1,count(w,0w0,a)))
      in c (is,0)
      end
    
    fun mksome (0w0, empty, empty) = empty
      | mksome t = some t
	
    fun union (empty, ns2) = ns2
      | union (ns1, empty) = ns1
      | union (some(w1, t11, t12), some(w2, t21, t22)) =
	some(Bits.orb(w1,w2), union(t11, t21), union(t12, t22))
	
    fun intersection (empty, ns2) = empty
      | intersection (ns1, empty) = empty
      | intersection (some(w1, t11, t12), some(w2, t21, t22)) =
	mksome(Bits.andb(w1,w2), intersection(t11, t21), intersection(t12, t22))
	
    fun difference (empty, ns2) = empty
      | difference (ns1, empty) = ns1
      | difference (some(w1, t11, t12), some(w2, t21, t22)) =
	mksome(Bits.andb(w1,Bits.notb w2), difference(t11, t21), difference(t12, t22))

    fun delete0(empty, n) = empty
      | delete0(some(w,t1,t2), n) =
         if n < bits_word then mksome(unsetb(w,n),t1,t2)
	 else if Bits.andb(n,0w1) <> 0w0 then mksome(w,delete0(t1, Bits.rshift(n-bits_word,0w1)), t2)
	 else mksome(w, t1, delete0(t2, Bits.rshift(n-bits_word-0w1,0w1)))
      
    fun delete(is,n) = if member(n,is) then delete0(is,n) else is

    fun disjoint (empty, ns2) = true
      | disjoint (ns1, empty) = true
      | disjoint (some(w1, t11, t12), some(w2, t21, t22)) =
	(Bits.andb(w1,w2) = 0w0) 
	andalso disjoint(t11, t21) 
	andalso disjoint(t12, t22)  

    fun foldset f (e, t) =
	let fun slb (n, d, w, a) =
	      let fun slb' (0w0,i,a) = a
		    | slb' (w,i,a) = if isb(w,i) then slb'(unsetb(w,i),i+0w1,f(a,n+d*i))
				     else slb'(w,i+0w1,a)
	      in slb' (w,0w0,a)
	      end     
	  fun sl (n, d, empty, a) = a
	    | sl (n, d, some(w, t1, t2), a) =
	      let val temp = n+d*bits_word
		  val d' = 0w2*d
	      in sl(temp, d', t1, 
		   sl(temp+d, d', t2, slb(n, d, w, a)))
	      end
	in sl(0w0, 0w1, t, e) 
	end

    fun mapset f t = foldset (fn (a,i) => f i :: a) ([], t)

    fun members t = foldset (fn (a,i) => i :: a) ([], t)

    fun natsetof []      = empty
      | natsetof (x::xs) = add(natsetof xs, x)

    fun fromto(i, j) = if i > j then empty else add0(fromto(i+0w1, j), i)

    fun setfilter p t =
        let
	  fun h' (n, d, w) =
	    let fun h'' (i,w') =
	          if i < bits_word then
		    if isb(w,i) andalso p(n+0w2*i) then h''(i+0w1, setb(w',i))
		    else h''(i+0w1,w')
		  else w'
	    in h'' (0w0,0w0)
	    end
	  fun h (n, d, empty) = empty
	    | h (n, d, some(w, t1, t2)) = 
		mksome(h' (n, d, w), h(n+d*bits_word, 0w2*d, t1), h(n+d*(bits_word+0w1), 0w2*d, t2))
	in h(0w0, 0w1, t) 
	end

    val size = cardinality

    val member = fn elt => fn set => member(elt,set)

    fun isEmpty empty = true
      | isEmpty _ = false

    fun eq s s' = (s=s')

    val list = members

    val fromList = natsetof

    fun addList [] set = set
      | addList (x::xs) set = add(addList xs set, x)

    fun insert elt set = add(set,elt)

    fun remove elt set = delete(set,elt)

    val difference = fn s1 => fn s2 => difference(s1,s2)

    fun intersect s1 s2 = intersection(s1,s2)

    val union = fn s1 => fn s2 => union(s1,s2)

    fun partition p t =
      let val t1 = setfilter p t
	  val t2 = setfilter (not o p) t
      in (t1,t2)
      end

    fun subst (a,b) s = (* subst elem b in s with a! *)
      if member b s then
	add(delete(s,b),a)
      else s

    fun fold f e s = foldset (fn (a,e) => f e a) (e,s)

    fun app f [] = ()
      | app f (x::xs) = (f x; app f xs)
    fun apply f s = app f (list s)

    fun map f = fromList o (mapset f)
end

(* Auxiliary functions for test cases *)

local
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Word
    in
	(from > to) orelse (p from) andalso (range (from+0w1, to) p)
    end;

fun list0 nil p = true
  | list0 (x::xs) p = p x andalso list0 xs p

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  
fun tstlist s l = (tst s) o list0 l

val _ = print "Testing implementation of sets using word structure...\n"

open NatSet
in	
fun mem w s = member s w
val s1 = fromList[0w100,0w101,0w102,0w103,0w104,0w105,0w106,0w107,0w108]
val s2 = fromList[0w106,0w107,0w108,0w109,0w110,0w111]
val s3 = union s1 s2
val s4 = intersect s1 s2
val s5 = difference s3 (fromList[0w104,0w105,0w106,0w107,0w161])

val _ = tstrange "test1" (0w100,0w108) (mem s1)
val _ = tstrange "test2" (0w109,0w200) (not o mem s1)

val _ = tstrange "test3" (0w100,0w111) (mem s3)
val _ = tstrange "test4" (0w112,0w200) (not o mem s3)

val _ = tstrange "test5" (0w100,0w105) (not o mem s4)
val _ = tstrange "test6" (0w106,0w108) (mem s4)
val _ = tstrange "test7" (0w109,0w200) (not o mem s4)
val _ = tst "test8" (eq(fromList [0w106,0w107,0w108]) s4)

val _ = tstrange "test9" (0w100,0w103) (mem s5)
val _ = tstrange "test10" (0w104,0w107) (not o mem s5)
val _ = tstrange "test11" (0w108,0w111) (mem s5)
val _ = tstrange "test12" (0w112,0w200) (not o mem s5)

val _ = tst "test13" (size s1 = 9 andalso size s2 = 6 andalso size s3 = 12 
		      andalso size s4 = 3 andalso size s5 = 8)

val s6 = remove 0w101 s5
val _ = tstrange "test14" (0w100,0w100) (mem s6)
val _ = tstrange "test15" (0w101,0w101) (not o mem s6)
val _ = tstrange "test16" (0w102,0w103) (mem s6)
val _ = tstrange "test17" (0w104,0w107) (not o mem s6)
val _ = tstrange "test18" (0w108,0w111) (mem s6)
val _ = tstrange "test19" (0w112,0w200) (not o mem s6)

fun T N str =
    let 
	fun mkN n = List.map Word.fromInt (Random.rangelist(0,N)(n,Random.newgen()))
	    
	val l7 = mkN 1000
	val l8 = mkN 1000
	val s7 = fromList l7
	val s8 = fromList l8
	    
	val _ = tstlist ("test20"^str) l7 (mem s7)
	val _ = tstlist ("test21"^str) l8 (mem s8)
	val _ = tstlist ("test22"^str) (l7@l8) (mem (union s7 s8))
	    
	val _ = tst ("test23"^str) (eq (union s7 s8)
				    (union(union(intersect s7 s8) 
					   (difference s7 s8)) 
				     (difference s8 s7)))
    in ()
    end

val _ = T 1000000 "a"
val _ = T 100000 "b"
val _ = T 10000 "c"
val _ = T 1000 "d"

val (s9,s10) = partition (fn w => Word.mod(w,0w2) = 0w0) s6
val _ = tst "test24" (fromList[0w100,0w102,0w108,0w110] = s9)
val _ = tst "test25" (fromList[0w103,0w109,0w111] = s10)

val _ = tst "test26" (fold (fn w1 => fn w2 => Word.+(w1,w2)) 0w0 s9 = 0w420)
val _ = tst "test27" (fold (fn w1 => fn w2 => Word.+(w1,w2)) 0w0 s10 = 0w323)

end