(* Lambda variables *)

structure Lvars: LVARS =
  struct
    structure PP = PrettyPrint

    (* Lambda variables are based on names which may be `matched'. In
     * particular, if two lambda variables, lv1 and lv2, are
     * successfully matched, eq(lv1,lv2) = true. This may affect the
     * canonical ordering of lambda variables. *)

    type name = Name.name

    type lvar = {name     : name,
		 str      : string,
		 free     : bool ref,
		 inserted : bool ref,
		 use      : int ref,
                 ubf64    : bool ref}

    fun new_named_lvar (str : string) : lvar =
        {name=Name.new(), str=str,
	 free=ref false, inserted=ref false,
	 use=ref 0, ubf64=ref false}

    fun newLvar () : lvar = new_named_lvar ""

    fun pr_lvar ({str="",name,ubf64,...} : lvar) : string = (if !ubf64 then "f" else "v") ^ Int.toString (#1(Name.key name))
      | pr_lvar {str,...} = str

    fun renew (lv:lvar) : lvar =
        {name=Name.new(), str=pr_lvar lv,
	 free=ref false, inserted=ref false,
	 use=ref 0, ubf64=ref (!(#ubf64 lv))}

    fun pr_lvar' ({str,name,ubf64,...} : lvar) : string =
	let val (i,s) = Name.key name
	    val str = if str = "" then (if !ubf64 then "f" else "v") else str
	in str ^ ":" ^ Int.toString i ^ ":" ^ s
	end

    fun name ({name,...} : lvar) : name = name

    fun key lv = Name.key (name lv)

    fun lt (lv1,lv2) = Name.lt(name lv1,name lv2)

    fun eq (lv1,lv2) = Name.eq(name lv1, name lv2)

    fun leq (lv1,lv2) = lt(lv1,lv2) orelse eq(lv1,lv2)

    fun str ({str,...}:lvar) : string = str

    fun usage ({use,...} : lvar) = use
    fun reset_use lv = usage lv := 0
    fun incr_use lv = usage lv := (!(usage lv) + 1)
    fun decr_use lv = usage lv := (!(usage lv) - 1)
    fun zero_use lv = !(usage lv) = 0
    fun one_use lv = !(usage lv) = 1

    fun set_ubf64 (lv:lvar) : unit = (#ubf64 lv) := true
    fun get_ubf64 (lv:lvar) : bool = !(#ubf64 lv)

    fun match (lv1,lv2) = Name.match(name lv1,name lv2)
    fun is_free ({free,...} : lvar) = free
    fun is_inserted ({inserted,...} : lvar) = inserted

    structure QD : QUASI_DOM =
      struct
	type dom = lvar
	type name = Name.name
	val name = name
	val pp = pr_lvar
      end

    structure Map = QuasiMap(QD)

    (* For the KAM machine *)
    val env_lvar = new_named_lvar("env")
    val notused_lvar = new_named_lvar("notused")

    val pu =
	Pickle.hashConsEq eq
	(Pickle.register "Lvars" [env_lvar,notused_lvar]
	 let fun to ((n,s,f),i,u,ubf64) : lvar =
		 {name=n, str=s, free=f, inserted=i, use=u, ubf64=ubf64}
	     fun from ({name=n, str=s, free=f, inserted=i, use=u, ubf64} : lvar) = ((n,s,f),i,u,ubf64)
	 in Pickle.newHash (#1 o Name.key o #name)
	     (Pickle.convert (to,from)
	      (Pickle.tup4Gen0(Pickle.tup3Gen0(Name.pu,Pickle.string,Pickle.refOneGen Pickle.bool),
			       Pickle.refOneGen Pickle.bool,
                               Pickle.refOneGen Pickle.int,
                               Pickle.refOneGen Pickle.bool)))
	 end)

  end


(***********************************************************************
  Applicative representation of finite sets of naturals, 1993-01-03
  sestoft@dina.kvl.dk
***********************************************************************)

structure Lvarset: LVARSET =
struct

  val xorb = Word.xorb
  val lshift = Word.<<
  val andb = Word.andb
  val notb = Word.notb
  val rshift = Word.>>
  val orb = Word.orb

    type lvar = Lvars.lvar

    datatype lvarset =
	LF
      | BR of lvar list * lvarset * lvarset    (* ordered list of lvars w.r.t. #2 o Lvars.key *)

    fun sing (0w0,lvar) = BR([lvar], LF, LF)
      | sing (n,lvar) = if andb(n,0w1) <> 0w0 then
	                BR(nil, sing(rshift(n,0w1),lvar), LF)
		 else
		     BR(nil, LF, sing(rshift(n,0w1) - 0w1, lvar))

    fun singleton lvar = sing (Word.fromInt(#1(Lvars.key lvar)),lvar)

    fun cardinality LF             = 0
      | cardinality (BR(b, t1, t2)) = List.length b + cardinality t1 + cardinality t2

    fun mkBR (nil, LF, LF) = LF
      | mkBR (b, t1, t2) = BR(b, t1, t2)

    fun mergeOr (nil,lvs) = lvs
      | mergeOr (lvs,nil) = lvs
      | mergeOr (lvs1' as (lv1::lvs1),lvs2' as (lv2::lvs2)) =
	let val s1 = #2 (Lvars.key lv1)
	    val s2 = #2 (Lvars.key lv2)
	in if s1 < s2 then lv1::mergeOr(lvs1,lvs2')
	   else if s2 < s1 then lv2::mergeOr(lvs1',lvs2)
		else lv2::mergeOr(lvs1,lvs2)
	end

    fun union (LF, ns2) = ns2
      | union (ns1, LF) = ns1
      | union (BR(b1, t11, t12), BR(b2, t21, t22)) =
	BR(mergeOr(b1,b2), union (t11, t21), union (t12, t22))

    fun add (set,lvar) = union(set, singleton lvar)

    fun mergeAnd (nil,_) = nil
      | mergeAnd (_,nil) = nil
      | mergeAnd (lvs1' as (lv1::lvs1),lvs2' as (lv2::lvs2)) =
	let val s1 = #2 (Lvars.key lv1)
	    val s2 = #2 (Lvars.key lv2)
	in if s1 < s2 then mergeAnd(lvs1,lvs2')
	   else if s2 < s1 then mergeAnd(lvs1',lvs2)
		else lv2::mergeAnd(lvs1,lvs2)
	end

    fun intersection (LF, ns2) = LF
      | intersection (ns1, LF) = LF
      | intersection (BR(b1, t11, t12), BR(b2, t21, t22)) =
	mkBR(mergeAnd(b1,b2), intersection(t11, t21), intersection(t12, t22))

    fun diff (nil,_) = nil
      | diff (lvs,nil) = lvs
      | diff (lvs1' as (lv1::lvs1),lvs2' as (lv2::lvs2)) =
	let val s1 = #2 (Lvars.key lv1)
	    val s2 = #2 (Lvars.key lv2)
	in if s1 < s2 then lv1 :: diff(lvs1,lvs2')
	   else if s2 < s1 then diff(lvs1',lvs2)
		else diff(lvs1,lvs2)
	end

    fun difference (LF, ns2) = LF
      | difference (ns1, LF) = ns1
      | difference (BR(b1, t11, t12), BR(b2, t21, t22)) =
	mkBR(diff(b1,b2), difference(t11, t21), difference(t12, t22))

    fun delete (is, i) = difference(is, singleton i)

    fun present(SOME _) = true
      | present NONE = false

    fun dis (nil,_) = true
      | dis (_,nil) = true
      | dis (lvs1' as (lv1::lvs1),lvs2' as (lv2::lvs2)) =
	let val s1 = #2 (Lvars.key lv1)
	    val s2 = #2 (Lvars.key lv2)
	in if s1 < s2 then dis(lvs1,lvs2')
	   else if s2 < s1 then dis(lvs1',lvs2)
		else false
	end

    fun disjoint (LF, ns2) = true
      | disjoint (ns1, LF) = true
      | disjoint (BR(b1, t11, t12), BR(b2, t21, t22)) =
	dis(b1,b2) andalso disjoint(t11, t21)
	andalso disjoint (t12, t22)

(*  fun member (i, is) = not(disjoint(is, singleton i)) *)

    fun member (lvar, is) =
	let val (i,s) = Lvars.key lvar
	    fun mems nil = false
	      | mems (x::xs) = #2 (Lvars.key x) = s orelse mems xs
	    fun mem (_, LF) = false
	      | mem (0w0, BR(b, _, _)) = mems b
	      | mem (n, BR(_, ns1, ns2)) =
	        if andb(n,0w1) <> 0w0 then
	             mem(rshift(n,0w1), ns1)
		 else
		     mem(rshift(n,0w1) - 0w1, ns2)
	in mem(Word.fromInt i, is) end

    fun lvarsetof []      = LF
      | lvarsetof (x::xs) = add(lvarsetof xs, x)

    fun foldset f (e, t) =
	let fun fo a b = foldl (fn (lv,a) => f(a,lv)) a b
	    fun sl (n, d, LF, a)                 = a
	      | sl (n, d, BR(b, LF, LF), a) = fo a b
	      | sl (n, d, BR(b, t1, LF), a) =
		sl(n+d, 2*d, t1, fo a b)
	      | sl (n, d, BR(b, t1, t2), a)    =
		sl(n+d, 2*d, t1, sl(n+2*d, 2*d, t2, fo a b))
	in sl(0, 1, t, e) end

    fun mapset f t = foldset (fn (a,i) => f i :: a) ([], t)

    fun members t = foldset (fn (a,i) => i :: a) ([], t)

    fun findLvar (pred: lvar -> 'a option) lvarset =
      let exception Found of (lvar * 'a)option
	  fun ss nil = ()
	    | ss (lv::lvs) = (case pred lv of
				SOME x => raise Found(SOME(lv,x))
			      | NONE => ss lvs)
          fun search LF = ()
            | search (BR(lvs, set1, set2)) =
               (ss lvs; search set1; search set2)
      in
        (search lvarset; NONE) handle Found result => result
      end

  val empty = LF

end
