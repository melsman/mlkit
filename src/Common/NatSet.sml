
(***********************************************************************
  NatSet

  Applicative representation of finite sets of naturals, 1993-01-03
  (Neat and fast solution using Bits operations)
  Peter Sestoft (sestoft@id.dth.dk), Department of Computer Science, 
  Technical University of Denmark, Building 344, DK-2800 Lyngby, Denmark

  Modified by Martin Elsman, Department of Computer Science,
  University of Copenhagen, 1996-06-08, to use bits in integers to
  hold containment information.
***********************************************************************)

functor NatSet(structure PP : PRETTYPRINT) : KIT_MONO_SET =
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

    type StringTree = PP.StringTree
    fun layoutSet {start:string, sep:string, finish:string} pr_elt s =
      PP.NODE{start=start,finish=finish,indent=0,childsep=PP.RIGHT sep,
	      children=map pr_elt (list s)}

    fun map f = fromList o (mapset f)

    fun die s = let val s = "Impossible.NatSet: " ^ s
		in print s ; raise Fail s
		end

    fun pu _ =
	let fun toInt empty = 0
	      | toInt (some _) = 1
	    val fun_empty = Pickle.con0 empty
	    fun fun_some pu =
		Pickle.con1 some (fn some a => a | _ => die "pu.some")
		(Pickle.tup3Gen(Pickle.word,pu,pu))
	in Pickle.dataGen (toInt,[fun_empty,fun_some])
	end
end

(***********************************************************************

Representation:

A finite set of naturals (non-negative integers) is represented by an
applicative array of boolean vectors (words), represented as a binary
tree (as in L.C. Paulson, "ML for the Working Programmer",
pp. 130-132).  Denoting the node (element) of t that has number n by
t[n], we have

	empty[n]	              = false
	some(w,t1,t2)[m < bits_word]  = isb(w,m)
	some(w,t1,t2)[2m+bits_word]   = t1[m]
	some(w,t1,t2)[2m+bits_word+1] = t2[m]


Complexity of the operations:

The operation empty takes constant time; the operations singleton i,
add(is,i), delete (i,is), and member(i, is) take time proportional to
log2(i); and the operations union(is1,is2), intersection(is1,is2),
difference(is1,is2) take time proportional to the cardinality of the
smaller set.  The operations members(is) and cardinality(is) take
time proportional to the size of is.

***********************************************************************)
