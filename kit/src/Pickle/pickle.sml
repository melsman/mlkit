(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07 
 *)

structure Pickle :> PICKLE =
  struct
    structure S = Stream
    structure H = Polyhash
    structure Dyn = EqHashDyn

    fun fail s = let val s = "Pickle." ^ s 
		 in print (s ^ "\n")
		  ; raise Fail s
		 end

    val maxDepth = 50
    val maxLength = 500
    fun hashAdd(w,a) = w + a*0w63
    fun stringHash s = 
	let val sz = size s
	    val sz = if sz > maxLength then maxLength else sz
	    fun loop (n,a) = if n >= sz then a
			     else loop (n+1, hashAdd(Word.fromInt(Char.ord(String.sub(s,n))),a))
	in loop (0,0w0)
	end

    fun hashCombine (h1,h2) d = 
	if d >= 0 then 0w0 
	else hashAdd(h1 (d-1), h2 (d-1))

    type dyn = Dyn.dyn

    type pe = (dyn, S.loc) H.hash_table
    type upe = (S.loc, dyn) H.hash_table

    type instream = S.IN S.stream * upe
    type outstream = S.OUT S.stream * pe
    type 'a pickler = 'a -> outstream -> outstream
    type 'a unpickler = instream -> 'a * instream
    type 'a hasher = 'a -> int -> word    (* int: hash depth *)
    type 'a eq = 'a * 'a -> bool

    type 'a pu = 'a pickler * 'a unpickler * 'a hasher * 'a eq

    fun pickler (pu:'a pu) = #1 pu
    fun unpickler (pu:'a pu) = #2 pu
    fun hasher (pu:'a pu) = #3 pu

    fun pu a = a

    fun wordGen (toWord:''a->word, fromWord:word->''a) : ''a pu =
       (fn w => fn (s, pe) => (S.outcw (toWord w,s), pe),
	fn (s, upe) => let val (w,s) = S.getcw s
		       in (fromWord w, (s, upe))
		       end,
        fn a => fn d => toWord a,
	op =)
	
    val word = wordGen (fn x => x, fn x => x)
    val int = wordGen (Word.fromInt, Word.toIntX)
    val bool = wordGen (fn true => 0w1 | false => 0w0,
			fn 0w0 => false | _ => true)
    val char = wordGen (Word.fromInt o Char.ord, 
			Char.chr o Word.toIntX)

    fun shareGen ((p,up,h,eq):'a pu) : 'a pu =
      let val REF = 0w0 and DEF = 0w1           
          val (toDyn,fromDyn) = Dyn.new h eq
      in
	 (fn v => fn (s, pe:pe) =>
            let val d = toDyn v
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF,s)
		       val s = S.outw(loc,s)
		   in (s,pe)
		   end
	       | NONE =>
		   let val s = S.outcw(DEF,s)
		       val loc = S.getLoc s
		   in  H.insert pe (d,loc)
		     ; p v (s, pe)
		   end
	    end,
	  fn (s, upe:upe) =>
	     let val (tag,s) = S.getcw s
	     in if tag = REF then
		   let val (loc,s) = S.getw s
		   in case H.peek upe loc of
			 SOME d => (fromDyn d, (s,upe))
		       | NONE => raise Fail "shareGen.impossible"
		   end
		else (* tag = DEF *)
		   let val loc = S.getLoc s
		       val (v,(s,upe)) = up (s,upe)
		   in H.insert upe (loc,toDyn v)
		    ; (v, (s,upe))
		   end
	     end,
	  h,
	  eq)
      end

    val string : string pu =
	shareGen
	(fn st => fn (s, pe) =>
	 let val sz = size st
	     val s = S.outcw(Word.fromInt sz, s)
	     val s = CharVector.foldl (fn (c,cs) => S.out(c,cs)) s st
	 in (s, pe)
	 end,
	 fn (s,upe) =>
	 let val (sz,s) = S.getcw s
	     val sz = Word.toInt sz
	     fun read (0,s,acc) = (implode(rev acc), s)
	       | read (n,s,acc) = 
		 let val (c,s) = S.get s
		 in read (n-1, s, c :: acc)
		 end
	     val (st,s) = read (sz,s,nil)
	 in  (st, (s, upe))
	 end,
	 fn s => fn d => stringHash s,
	 op =)

    fun pairGen ((p1,up1,h1,eq1):'a pu, (p2,up2,h2,eq2):'b pu) : ('a * 'b) pu = shareGen
      (fn (v1:'a,v2:'b) => fn os =>
	  let val os = p1 v1 os
	  in p2 v2 os
	  end,
       fn is =>
          let val (v1,is) = up1 is
	      val (v2,is) = up2 is
	  in ((v1,v2), is)
	  end,
       fn (a,b) => hashCombine (h1 a, h2 b),
       fn ((a1,a2),(b1,b2)) => eq1(a1,b1) andalso eq2(a2,b2)
       )

    fun refGen ((p,up,h,_):'a pu) (v_dummy:'a) : 'a ref pu =
      let val REF_LOC = 0w0 and REF_DEF = 0w1           
	  fun href (ref a) = hashCombine (fn _ => 0w1, h a)
          val (toDyn,fromDyn) = Dyn.new href (op=)
      in
	 (fn r as ref v => fn (s, pe:pe) =>
            let val d = toDyn r
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF_LOC,s)
		       val s = S.outw(loc,s)
		   in (s,pe)
		   end
	       | NONE =>
		   let val s = S.outcw(REF_DEF,s)
		       val loc = S.getLoc s
		   in H.insert pe (d,loc)
		    ; p v (s, pe)
		   end
	    end,
	  fn (s, upe:upe) =>
	     let val (tag,s) = S.getcw s
	     in if tag = REF_LOC then
		   let val (loc,s) = S.getw s
		   in case H.peek upe loc of
			 SOME d => (fromDyn d, (s, upe))
		       | NONE => raise Fail "ref.impossible"
		   end
		else (* tag = REF_DEF *)
		   let val loc = S.getLoc s
		       val r = ref v_dummy
		       val _ = H.insert upe (loc,toDyn r)
		       val (v,(s,upe)) = up (s, upe)
		   in r := v ; (r, (s, upe))
		   end
	     end,
	  href,
	  op =
	 )
      end

    fun ref0Gen ((p,up,h,_):'a pu) : 'a ref pu =
      let val REF_LOC = 0w0 and REF_DEF = 0w1           
	  fun href (ref a) = hashCombine (fn _ => 0w1, h a)
          val (toDyn,fromDyn) = Dyn.new href (op=)
      in
	 (fn r as ref v => fn (s, pe:pe) =>
            let val d = toDyn r
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF_LOC,s)
		       val s = S.outw(loc,s)
		   in (s,pe)
		   end
	       | NONE =>
		   let val s = S.outcw(REF_DEF,s)
		       val loc = S.getLoc s
		   in H.insert pe (d,loc)
		    ; p v (s, pe)
		   end
	    end,
	  fn (s, upe:upe) =>
	     let val (tag,s) = S.getcw s
	     in if tag = REF_LOC then
		   let val (loc,s) = S.getw s
		   in case H.peek upe loc of
			 SOME d => (fromDyn d, (s, upe))
		       | NONE => raise Fail "ref.impossible"
		   end
		else (* tag = REF_DEF *)
		   let val loc = S.getLoc s
		       val (v,(s,upe)) = up (s, upe)
		       val r = ref v
		   in H.insert upe (loc,toDyn r) 
		    ; (r, (s, upe))
		   end
	     end,
	  href,
	  op =
	 )
      end

    fun dataGen (toInt: 'a -> int, eq: 'a * 'a -> bool, 
		 fs : ('a pu -> 'a pu) list) : 'a pu =
	let val res : 'a pu option ref = ref NONE
	    val ps : 'a pu Vector.vector option ref = ref NONE
	    fun p v (s,pe) =
	      let val i = toInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(getPUPI i) v (s,pe)
	      end
            and up (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(getPUPI (Word.toInt w)) (s,upe)
	      end
	    and getPUP() = 
		case !res of
		    NONE => let val pup = shareGen (p,up,h,eq)
			    in res := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and getPUPI (i:int) =
		case !ps of
		    NONE => let val ps0 = map (fn f => f (getPUP())) fs
				val psv = Vector.fromList ps0
			    in ps := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)
	    and h v d =
		let val i = toInt v
		    val h_arg = #3 (getPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
        in getPUP()
	end

    fun data2Gen (aToInt: 'a -> int, aEq : 'a * 'a -> bool, afs : ('a pu * 'b pu -> 'a pu) list,
		  bToInt: 'b -> int, bEq : 'b * 'b -> bool, bfs : ('a pu * 'b pu -> 'b pu) list) 
	: 'a pu * 'b pu =
	let val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector option ref = ref NONE
	    val bPs : 'b pu Vector.vector option ref = ref NONE
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(aGetPUPI (Word.toInt w)) (s,upe)
	      end
	    and aGetPUP() = 
		case !aRes of
		    NONE => let val pup = shareGen (aP,aUp,aH,aEq)
			    in aRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and aGetPUPI (i:int) =
		case !aPs of
		    NONE => let val ps0 = map (fn f => f (aGetPUP(),bGetPUP())) afs
				val psv = Vector.fromList ps0
			    in aPs := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)		
	    and bP v (s,pe) =
	      let val i = bToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(bGetPUPI (Word.toInt w)) (s,upe)
	      end
	    and bGetPUP() = 
		case !bRes of
		    NONE => let val pup = shareGen (bP,bUp,bH,bEq)
			    in bRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and bGetPUPI (i:int) =
		case !bPs of
		    NONE => let val ps0 = map (fn f => f (aGetPUP(),bGetPUP())) bfs
				val psv = Vector.fromList ps0
			    in bPs := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)
	    and aH v d =
		let val i = aToInt v
		    val h_arg = #3 (aGetPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
	    and bH v d =
		let val i = bToInt v
		    val h_arg = #3 (bGetPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
        in (aGetPUP(), bGetPUP())
	end

    fun data3Gen (aToInt: 'a -> int, aEq : 'a * 'a -> bool, afs : ('a pu * 'b pu * 'c pu -> 'a pu) list,
		  bToInt: 'b -> int, bEq : 'b * 'b -> bool, bfs : ('a pu * 'b pu * 'c pu -> 'b pu) list,
		  cToInt: 'c -> int, cEq : 'c * 'c -> bool, cfs : ('a pu * 'b pu * 'c pu -> 'c pu) list) 
	: 'a pu * 'b pu * 'c pu =
	let val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val cRes : 'c pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector option ref = ref NONE
	    val bPs : 'b pu Vector.vector option ref = ref NONE
	    val cPs : 'c pu Vector.vector option ref = ref NONE
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(aGetPUPI (Word.toInt w)) (s,upe)
	      end
	    and aGetPUP() = 
		case !aRes of
		    NONE => let val pup = shareGen (aP,aUp,aH,aEq)
			    in aRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and aGetPUPI (i:int) =
		case !aPs of
		    NONE => let val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) afs
				val psv = Vector.fromList ps0
			    in aPs := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)		
	    and bP v (s,pe) =
	      let val i = bToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(bGetPUPI (Word.toInt w)) (s,upe)
	      end
	    and bGetPUP() = 
		case !bRes of
		    NONE => let val pup = shareGen (bP,bUp,bH,bEq)
			    in bRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and bGetPUPI (i:int) =
		case !bPs of
		    NONE => let val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) bfs
				val psv = Vector.fromList ps0
			    in bPs := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)
	    and cP v (s,pe) =
	      let val i = cToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in #1(cGetPUPI i) v (s,pe)
	      end
            and cUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(cGetPUPI (Word.toInt w)) (s,upe)
	      end
	    and cGetPUP() = 
		case !cRes of
		    NONE => let val pup = shareGen (cP,cUp,cH,cEq)
			    in cRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and cGetPUPI (i:int) =
		case !cPs of
		    NONE => let val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) cfs
				val psv = Vector.fromList ps0
			    in cPs := SOME psv
			     ; Vector.sub(psv,i)
			    end 
		  | SOME psv => Vector.sub(psv,i)
	    and aH v d =
		let val i = aToInt v
		    val h_arg = #3 (aGetPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
	    and bH v d =
		let val i = bToInt v
		    val h_arg = #3 (bGetPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
	    and cH v d =
		let val i = cToInt v
		    val h_arg = #3 (cGetPUPI i)
		in hashCombine (fn _ => Word.fromInt i, h_arg v) d
		end
        in (aGetPUP(), bGetPUP(), cGetPUP())
	end

    fun con0 (eq: 'b*'b->bool) (b: 'b) (pu: 'b pu) =
	(fn _ => fn spe => spe,
	 fn supe => (b,supe),
	 fn _ => fn _ => 0w0,
	 eq)

    fun con1 (eq: 'b*'b->bool) 
	(con:'a->'b) (decon: 'b->'a) (pu: 'a pu) =
	(fn b:'b => pickler pu (decon b),
	 fn supe => 
	 let val (a,supe) = unpickler pu supe
	 in (con a,supe)
	 end,
	 fn b:'b => hasher pu (decon b),
	 eq)

    fun listGen (pu_a as (p_a,up_a,h_a,eq_a): 'a pu) : 'a list pu =
	let fun toInt nil = 0
	      | toInt _ = 1
	    fun eq (nil,nil) = true
	      | eq (x::xs,y::ys) = eq_a(x,y) andalso eq (xs,ys)
	      | eq _ = false
	    val f_nil = con0 eq nil
	    fun f_cons pu =
		con1 eq (op ::) (fn op :: p => p | _ => fail "cons")
		(pairGen(pu_a,pu))
(*
	    fun f_nil (_ : 'a list pu) : 'a list pu = 
		(fn _ => fn spe => spe,
		 fn supe => (nil,supe),
		 fn _ => fn _ => 0w0,
		 eq)
	    fun f_cons ((p, up, h, eq): 'a list pu) : 'a list pu =
		(fn l => fn spe =>
		 case l of v_a :: v_r =>
		 let val spe = p_a v_a spe
		 in p v_r spe
		 end
	         | _ => raise Fail "f_cons",
		 fn supe =>
		 let val (v_a,supe) = up_a supe
		     val (v_r,supe) = up supe
		 in (v_a::v_r, supe)
		 end,
		 fn v_a::rest => hashCombine (h_a v_a, h rest)
		  | _ => raise Fail "f_cons",
		 eq)
*)
	in dataGen (toInt,eq,[f_nil,f_cons])
	end

    fun optionGen (pu_a: 'a pu) : 'a option pu =
	let fun toInt NONE = 0
	      | toInt (SOME _) = 1
	    fun eq (NONE,NONE) = true
	      | eq (SOME a1,SOME a2) = #4 pu_a (a1,a2)
	      | eq _ = false
	    val fun_NONE = con0 eq NONE
	    fun fun_SOME _ = 
		con1 eq SOME (fn SOME v => v | NONE => fail "option") pu_a
	in dataGen(toInt,eq,[fun_NONE,fun_SOME])
	end
(*
    fun optionGen ((p,up,h,eq):'a pu) : 'a option pu = shareGen
      let val N = 0w0
	  val S = 0w1
      in
	  (fn v => fn (s,pe) =>
	   case v of
	       NONE => (S.outcw(N,s),pe)
	     | SOME v => 
	      let val s = S.outcw(S,s)
	      in p v (s,pe)
	      end,
	   fn (s,upe) =>
	   let val (w,s) = S.getcw s
	   in if w = N then (NONE,(s,upe))
	      else let val (v,is) = up (s,upe)
		   in (SOME v,is)
		   end
	   end,
	   fn NONE => (fn _ => N)
	    | SOME a => (hashCombine (fn _ => S, h a)),
	   fn (NONE,NONE) => true
	    | (SOME a1, SOME a2) => eq (a1,a2)
	    | _ => false)
      end

    fun ref0Gen ((p_a,up_a,h_a): ''a pu) : ''a ref pu =
	let fun toInt _ = 0
	    fun f_ref (_ : ''a ref pu) : ''a ref pu =
		(fn ref v_a => fn spe => p_a v_a spe,
		 fn supe =>
		 let val (v_a,supe) = up_a supe
		 in (ref v_a, supe)
		 end,
		 fn ref v_a => fn d => h_a v_a)
	in dataGen (toInt,[f_ref])
	end
*)
    fun enumGen (xs: ''a list) : ''a pu =   (*inefficient*)
	let val (wxs,n) = 
	    List.foldl (fn (x, (wxs, n)) => 
			((x,Word.fromInt n)::wxs, n+1)) 
	    (nil,0) xs
	    fun lookupw nil _ = raise Fail "enumGen.unknown constructor"
	      | lookupw ((x,w)::xs) v = if x=v then w else lookupw xs v
	    fun lookupv nil _ = raise Fail "enumGen.unknown constructor tag"
	      | lookupv ((x,w)::xs) w0 = if w=w0 then x else lookupv xs w0
	in
	    (fn v => fn (s,pe) => (S.outcw(lookupw wxs v,s),pe),
	     fn (s, upe) =>
	     let val (w,s) = S.getcw s
	     in (lookupv wxs w, (s,upe))
	     end,
	     fn v => fn d => lookupw wxs v,
	     op =
	    )
	end

    exception PickleExn
    fun fromString (s : string) : instream  = 
	(S.openIn s, 
	 H.mkTable (Word.toIntX, op =) (10,PickleExn))

    fun toString ((os,pe):outstream) : string = S.toString os
	
    fun empty() : outstream = 
	(S.openOut(), 
	 H.mkTable (Word.toIntX o Dyn.hash maxDepth, Dyn.eq) (10,PickleExn))

    fun get (s,upe) = 
	let val (w,s) = S.getcw s
	in (w, (s,upe))
	end
    fun out (w,(s,pe)) =
	let val s = S.outcw(w,s)
	in (s,pe)
	end

    fun convert (to,back) (p,up,h,eq) =
	shareGen
	(fn v => fn s => p (back v) s,
	 fn s => let val (v,s) = up s
		 in (to v,s)
		 end,
	 h o back,
	 fn (x,y) => eq(back x, back y))

    fun tup3Gen (a,b,c) =
	let fun to (a,(b,c)) = (a,b,c)
	    fun from (a,b,c) = (a,(b,c))
	in convert (to,from) (pairGen(a,pairGen(b,c)))
	end

    fun tup4Gen (a,b,c,d) =
	let fun to ((a,b),(c,d)) = (a,b,c,d)
	    fun from (a,b,c,d) = ((a,b),(c,d))
	in convert (to,from) (pairGen(pairGen(a,b),pairGen(c,d)))
	end

    val real = convert (fn v => case Real.fromString v of
			SOME v => v
		      | NONE => fail "real.convert",
			    Real.toString) 
	string

    val time = convert (Time.fromReal,Time.toReal) real

    val unit =
	(fn () => fn spe => spe,
	 fn supe => ((),supe),
	 fn () => fn _ => 0w0,
	 fn _ => true)

    fun cache (f:'a -> 'b pu) : 'a -> 'b pu =
	let val cache : 'b pu option ref = ref NONE
	in fn a : 'a =>
	    case !cache of
		SOME pu => pu
	      | NONE => let val pu = f a
			in cache := SOME pu
			 ; pu
			end
	end
  end

