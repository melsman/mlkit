(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07 
 *)

structure Pickle :> PICKLE =
  struct
    val sharing_p = true
    val debug_p = false

    structure S = Stream
    structure H = Polyhash
    structure Dyn = EqHashDyn

    fun fail s = let val s = "Pickle." ^ s 
		 in print (s ^ "\n")
		  ; raise Fail s
		 end

    val maxDepth = 10
    val maxLength = 500

    local
	val Alpha = 0w65599
	val Beta = 0w19
    in
	fun hashComb a f d =
	    if d <= 0 then a else f a (d-1)
	fun hashAdd(w,acc) = w + acc*Alpha
	fun hashAddSmall(w,acc) = w + acc*Beta
	val REF_HASH = 0w5
	val ENUM_HASH = 0w7
	val CON0_HASH = 0w11
	val CON1_HASH = 0w13
	val DAT_HASH = 0w17
	val PAIR_HASH = 0w19
	val UNIT_HASH = 0w23
    end

    fun stringHash acc s = 
	let val sz = size s
	    val sz = if sz > maxLength then maxLength else sz
	    fun loop (n,a) = if n >= sz then a
			     else loop (n+1, hashAddSmall(Word.fromInt(Char.ord(String.sub(s,n))),a))
	in loop (0,acc)
	end

    type dyn = Dyn.dyn

    type pe = (dyn, S.loc) H.hash_table
    type upe = (S.loc, dyn) H.hash_table

    type instream = S.IN S.stream * upe
    type outstream = S.OUT S.stream * pe
    type 'a pickler = 'a -> outstream -> outstream
    type 'a unpickler = instream -> 'a * instream
    type 'a hasher = 'a -> word -> int -> word    (* int: hash depth *)
    type 'a eq = 'a * 'a -> bool

    type 'a pu = 'a pickler * 'a unpickler * 'a hasher * 'a eq

    fun pickler (pu:'a pu) = #1 pu
    fun unpickler (pu:'a pu) = #2 pu
    fun hasher (pu:'a pu) = #3 pu

    fun pu a = a

    fun w32_to_w w32 = (Word.fromLargeWord o Word32.toLargeWord) w32
    fun w_to_w32 w = (Word32.fromLargeWord o Word.toLargeWord) w

    local 
	val counter : Word32.word ref = ref 0w0
	fun new() : Word32.word= !counter before counter := !counter + 0w1
    in
	fun debug str (pu:'a pu) : 'a pu =
	    if not debug_p then pu
	    else
	    let val c = new()
	    in (fn v => fn (s,pe) =>
		let val s = S.outw (c,s)
		in pickler pu v (s,pe)
		end,
		fn (s,upe) => 
		let val (w,s) = S.getw s
		in if w <> c then fail ("debug.expected " ^ str)
		   else unpickler pu (s,upe)
		end,
		hasher pu,
		#4 pu)
	    end
    end

    fun wordGen s (toWord:''a->Word32.word, fromWord:Word32.word->''a) : ''a pu =
	debug s
       (fn w => fn (s, pe) => (S.outcw (toWord w,s), pe),
	fn (s, upe) => let val (w,s) = S.getcw s
		       in (fromWord w, (s, upe))
		       end,
        fn a => fn acc => fn d => hashAdd(w32_to_w(toWord a),acc),
	op =)
	
    val word = wordGen "word" (w_to_w32, w32_to_w)
    val word32 = wordGen "word32" (fn x => x, fn x => x)
    val int = wordGen "int" (Word32.fromInt, Word32.toIntX)
    val int32 = wordGen "int32" (Word32.fromLargeInt o Int32.toLarge, Int32.fromLarge o Word32.toLargeIntX)
	
    val bool = wordGen "bool" (fn true => 0w1 | false => 0w0,
			       fn 0w0 => false | _ => true)
    val char = wordGen "char" (Word32.fromInt o Char.ord, 
			       Char.chr o Word32.toIntX)

    fun shareGen (pu as (p,up,h,eq):'a pu) : 'a pu =
      if not sharing_p then pu else
      debug "shareGen"
      let val REF = 0w0 and DEF = 0w1           
          val (toDyn,fromDyn) = Dyn.new (fn v => h v 0w0) eq
      in
	 (fn v => fn (s, pe:pe) =>
            let val d = toDyn v
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF,s)
		       val s = S.outw(w_to_w32 loc,s)
		   in (s,pe)
		   end
	       | NONE =>
		   let val s = S.outcw(DEF,s)
		       val loc = S.getLoc s
		       val res = p v (s, pe)   (* do insert after the pickling    *)
		   in  H.insert pe (d,loc)     (*  - otherwise there are problems *)
		     ; res                     (*    with cycles.                 *)
		   end
	    end,
	  fn (s, upe:upe) =>
	     let val (tag,s) = S.getcw s
	     in if tag = REF then
		   let val (loc,s) = S.getw s
		       val loc' = w32_to_w loc
		   in case H.peek upe loc' of
			 SOME d => (fromDyn d, (s,upe))
		       | NONE => fail ("shareGen.impossible, loc=" ^ Word32.toString loc ^ ", loc'=" ^ Word.toString loc')
		   end
		else if tag = DEF then
		   let val loc = S.getLoc s
		       val (v,(s,upe)) = up (s,upe)
		       val _ = case H.peek upe loc of
			   NONE => ()
			 | SOME _ => fail ("shareGen.Location " ^ Word.toString loc ^ " already there!")
		   in H.insert upe (loc,toDyn v)
		    ; (v, (s,upe))
		   end
		else fail "shareGen.impossible2"
	     end,
	  h,
	  eq)
      end

    val string : string pu =
	(shareGen o debug "string")
	(fn st => fn (s, pe) =>
	 let val sz = size st
	     val s = S.outcw(Word32.fromInt sz, s)
	     val s = CharVector.foldl (fn (c,cs) => S.out(c,cs)) s st
	 in (s, pe)
	 end,
	 fn (s,upe) =>
	 let val (sz,s) = S.getcw s
	     val sz = Word32.toInt sz
	     fun read (0,s,acc) = (implode(rev acc), s)
	       | read (n,s,acc) = 
		 let val (c,s) = S.get s
		 in read (n-1, s, c :: acc)
		 end
	     val (st,s) = read (sz,s,nil)
	 in  (st, (s, upe))
	 end,
	 fn s => fn acc => fn d => stringHash acc s,
	 op =)

    fun pairGen ((p1,up1,h1,eq1):'a pu, (p2,up2,h2,eq2):'b pu) : ('a * 'b) pu = 
      (shareGen o debug "pair")
      (fn (v1:'a,v2:'b) => fn os =>
	  let val os = p1 v1 os
	  in p2 v2 os
	  end,
       fn is =>
          let val (v1,is) = up1 is
	      val (v2,is) = up2 is
	  in ((v1,v2), is)
	  end,
       fn (a,b) => fn acc => fn d => 
       let val acc = hashAddSmall(PAIR_HASH,acc)
       in hashComb (hashComb acc (h2 b) d) (h1 a) d
       end,
       fn ((a1,a2),(b1,b2)) => eq1(a1,b1) andalso eq2(a2,b2)
       )

    fun refGen ((p,up,h,_):'a pu) (v_dummy:'a) : 'a ref pu =
      debug "refGen"
      let val REF_LOC = 0w0 and REF_DEF = 0w1           
	  fun href (ref a) acc = 
	      hashComb (hashAddSmall(REF_HASH, acc)) (h a)
          val (toDyn,fromDyn) = Dyn.new (fn v => href v 0w0) (op=)
      in
	 (fn r as ref v => fn (s, pe:pe) =>
            let val d = toDyn r
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF_LOC,s)
		       val s = S.outw(w_to_w32 loc,s)
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
		   in case H.peek upe (w32_to_w loc) of
			 SOME d => (fromDyn d, (s, upe))
		       | NONE => fail "ref.impossible"
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
      debug "ref0Gen"
      let val REF_LOC = 0w0 and REF_DEF = 0w1           
	  fun href (ref a) acc = hashComb (hashAddSmall(REF_HASH,acc)) (h a)
          val (toDyn,fromDyn) = Dyn.new (fn v => href v 0w0) (op=)
      in
	 (fn r as ref v => fn (s, pe:pe) =>
            let val d = toDyn r
	    in case H.peek pe d of
		 SOME loc => 
		   let val s = S.outcw(REF_LOC,s)
		       val s = S.outw(w_to_w32 loc,s)
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
		   in case H.peek upe (w32_to_w loc) of
			 SOME d => (fromDyn d, (s, upe))
		       | NONE => fail "ref.impossible"
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

    fun dataGen (toInt: 'a -> int, fs : ('a pu -> 'a pu) list) : 'a pu =
	debug "dataGen"
	let val res : 'a pu option ref = ref NONE
	    val ps : 'a pu Vector.vector option ref = ref NONE
	    fun p v (s,pe) =
	      let val i = toInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(getPUPI i) v (s,pe)
	      end
            and up (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(getPUPI (Word32.toInt w)) (s,upe)
	      end
	    and eq(a1:'a,a2:'a) : bool =
		let val n = toInt a1
		in n = toInt a2 andalso #4 (getPUPI n) (a1,a2)
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
	    and h v acc =
		let val i = toInt v
		    val h_arg = #3 (getPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
        in getPUP()
	end

    fun data2Gen (aToInt: 'a -> int, afs : ('a pu * 'b pu -> 'a pu) list,
		  bToInt: 'b -> int, bfs : ('a pu * 'b pu -> 'b pu) list) 
	: 'a pu * 'b pu =
	let val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector option ref = ref NONE
	    val bPs : 'b pu Vector.vector option ref = ref NONE
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(aGetPUPI (Word32.toInt w)) (s,upe)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		let val n = aToInt a1
		in n = aToInt a2 andalso #4 (aGetPUPI n) (a1,a2)
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
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(bGetPUPI (Word32.toInt w)) (s,upe)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		let val n = bToInt b1
		in n = bToInt b2 andalso #4 (bGetPUPI n) (b1,b2)
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
	    and aH v acc =
		let val i = aToInt v
		    val h_arg = #3 (aGetPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
	    and bH v acc =
		let val i = bToInt v
		    val h_arg = #3 (bGetPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
        in (debug "data2Gen.a" (aGetPUP()), debug "data2Gen.b" (bGetPUP()))
	end

    fun data3Gen (aToInt: 'a -> int, afs : ('a pu * 'b pu * 'c pu -> 'a pu) list,
		  bToInt: 'b -> int, bfs : ('a pu * 'b pu * 'c pu -> 'b pu) list,
		  cToInt: 'c -> int, cfs : ('a pu * 'b pu * 'c pu -> 'c pu) list) 
	: 'a pu * 'b pu * 'c pu =
	let val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val cRes : 'c pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector option ref = ref NONE
	    val bPs : 'b pu Vector.vector option ref = ref NONE
	    val cPs : 'c pu Vector.vector option ref = ref NONE
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(aGetPUPI (Word32.toInt w)) (s,upe)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		let val n = aToInt a1
		in n = aToInt a2 andalso #4 (aGetPUPI n) (a1,a2)
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
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(bGetPUPI (Word32.toInt w)) (s,upe)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		let val n = bToInt b1
		in n = bToInt b2 andalso #4 (bGetPUPI n) (b1,b2)
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
		  val s = S.outcw (Word32.fromInt i, s)
	      in #1(cGetPUPI i) v (s,pe)
	      end
            and cUp (s,upe) =
	      let val (w,s) = S.getcw s
	      in #2(cGetPUPI (Word32.toInt w)) (s,upe)
	      end
	    and cEq(c1:'c,c2:'c) : bool =
		let val n = cToInt c1
		in n = cToInt c2 andalso #4 (cGetPUPI n) (c1,c2)
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
	    and aH v acc =
		let val i = aToInt v
		    val h_arg = #3 (aGetPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
	    and bH v acc =
		let val i = bToInt v
		    val h_arg = #3 (bGetPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
	    and cH v acc =
		let val i = cToInt v
		    val h_arg = #3 (cGetPUPI i)
		in hashComb(hashAddSmall(Word.fromInt i,hashAddSmall(DAT_HASH,acc))) (h_arg v)
		end
        in (debug "data3Gen.a" (aGetPUP()), debug "data3Gen.b" (bGetPUP()), debug "data3Gen.c" (cGetPUP()))
	end

    fun con0 (b: 'b) (pu: 'b pu) =
	(fn _ => fn spe => spe,
	 fn supe => (b,supe),
	 fn _ => fn acc => fn d => hashAddSmall(CON0_HASH,acc),
	 fn _ => true)       (* tag is checked with toInt in dataNGen *)

    fun con1 (con:'a->'b) (decon: 'b->'a) (pu: 'a pu) =
	(fn b:'b => pickler pu (decon b),
	 fn supe => 
	 let val (a,supe) = unpickler pu supe
	 in (con a,supe)
	 end,
	 fn b:'b => fn acc => hashComb (hashAddSmall(CON1_HASH,acc)) (hasher pu (decon b)),
	 fn (b1:'b, b2:'b) => #4 pu (decon b1, decon b2))

    fun listGen (pu_a as (p_a,up_a,h_a,eq_a): 'a pu) : 'a list pu =
	debug "list"
	let fun toInt nil = 0
	      | toInt (op :: _) = 1
	    val f_nil = con0 nil
	    fun f_cons pu =
		con1 (op ::) (fn op :: p => p | _ => fail "cons")
		(pairGen(pu_a,pu))
	in dataGen (toInt,[f_nil,f_cons])
	end

    fun optionGen (pu_a: 'a pu) : 'a option pu =
	debug "option"
	let fun toInt NONE = 0
	      | toInt (SOME _) = 1
	    val fun_NONE = con0 NONE
	    fun fun_SOME _ = 
		con1 SOME (fn SOME v => v | NONE => fail "option") pu_a
	in dataGen(toInt,[fun_NONE,fun_SOME])
	end

    fun enumGen (xs: ''a list) : ''a pu =   (*inefficient*)
	debug "enum"
	let val (wxs,n) = 
	    List.foldl (fn (x, (wxs, n)) => 
			((x,Word.fromInt n)::wxs, n+1)) 
	    (nil,0) xs
	    fun lookupw nil _ = fail "enumGen.unknown constructor"
	      | lookupw ((x,w)::xs) v = if x=v then w else lookupw xs v
	    fun lookupv nil _ = fail "enumGen.unknown constructor tag"
	      | lookupv ((x,w)::xs) w0 = if w=w0 then x else lookupv xs w0
	in
	    (fn v => fn (s,pe) => (S.outcw(w_to_w32(lookupw wxs v),s),pe),
	     fn (s, upe) =>
	     let val (w,s) = S.getcw s
	     in (lookupv wxs (w32_to_w w), (s,upe))
	     end,
	     fn v => fn acc => fn d => hashAddSmall(ENUM_HASH,hashAddSmall(lookupw wxs v,acc)),
	     op =
	    )
	end

    exception PickleExn
    fun fromString (s : string) : instream  = 
	(S.openIn s, 
	 H.mkTable (Word.toIntX, op =) (10,PickleExn))

    fun toString ((os,pe):outstream) : string = 
	let val res = S.toString os
(*
	    val l = H.bucketSizes pe
	    val _ = print ("Buckets: " ^ Int.toString (length l) ^ "\n")
	    val _ = app (fn i => print ("  " ^ Int.toString i)) l
*)
	in res
	end
	
    fun empty() : outstream = 
	(S.openOut(), 
	 H.mkTable (Word.toIntX o Dyn.hash maxDepth, Dyn.eq) (10,PickleExn))

    fun convert (to,back) (p,up,h,eq) =
	shareGen
	(fn v => fn s => p (back v) s,
	 fn s => let val (v,s) = up s
		 in (to v,s)
		 end,
	 fn v => fn acc => hashComb (hashAddSmall(0w1,acc)) ((h o back) v),
	 fn (x,y) => eq(back x, back y))

    fun tup3Gen (a,b,c) =
	debug "tup3"
	let fun to (a,(b,c)) = (a,b,c)
	    fun from (a,b,c) = (a,(b,c))
	in convert (to,from) (pairGen(a,pairGen(b,c)))
	end

    fun tup4Gen (a,b,c,d) =
	debug "tup4"
	let fun to ((a,b),(c,d)) = (a,b,c,d)
	    fun from (a,b,c,d) = ((a,b),(c,d))
	in convert (to,from) (pairGen(pairGen(a,b),pairGen(c,d)))
	end

    fun vectorGen pu =
	convert (Vector.fromList,Vector.foldr (op ::) nil)
	(listGen pu)

    val real = 
	debug "real"
	(convert (fn v => case Real.fromString v of
		  SOME v => v
		| NONE => fail "real.convert",
		      Real.toString) 
	 string)

    val time = 
	debug "time" (convert (Time.fromReal,Time.toReal) real)

    val unit =
	(fn () => fn spe => spe,
	 fn supe => ((),supe),
	 fn () => fn acc => fn _ => hashAddSmall(UNIT_HASH,acc),
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

    fun register (vs: 'a list) (pu : 'a pu) : 'a pu =
	let val h : ('a,word) H.hash_table = H.mkTable (fn v => Word.toIntX (hasher pu v 0w0 maxDepth), #4 pu) (10,PickleExn)
	    val _ = List.foldl (fn (e,n) => (H.insert h (e,n); n + 0w1)) 0w1 vs
	    val v = Vector.fromList vs
	    fun lookup w = 
		let val i = Word.toInt w - 1
		in Vector.sub(v,i)
		end		    
	    val NOT_THERE : word = 0w0
	in (fn v => fn (s,pe) => 
	    case H.peek h v of
		SOME w => (S.outcw(w_to_w32 w,s),pe)
	      | NONE => let val s = S.outcw(w_to_w32 NOT_THERE,s)
			in pickler pu v (s,pe)
			end,
	    fn (s,upe) =>
	    let val (w,s) = S.getcw s
		val w = w32_to_w w
	    in if w = NOT_THERE then unpickler pu (s,upe)
	       else let val v = lookup w
		    in (v,(s,upe))
		    end
	    end,
	    hasher pu,
	    #4 pu)	   
	end

    fun registerEq (eq: 'a * 'a -> bool) (key : 'a -> int) (vs: 'a list) (pu : 'a pu) : 'a pu =
	let val h : ('a,word) H.hash_table = H.mkTable (key, eq) (10,PickleExn)
	    val _ = List.foldl (fn (e,n) => (H.insert h (e,n); n + 0w1)) 0w1 vs
	    val v = Vector.fromList vs
	    fun lookup w = 
		let val i = Word.toInt w - 1
		in Vector.sub(v,i)
		end		    
	    val NOT_THERE : word = 0w0
	in (fn v => fn (s,pe) => 
	    case H.peek h v of
		SOME w => (S.outcw(w_to_w32 w,s),pe)
	      | NONE => let val s = S.outcw(w_to_w32 NOT_THERE,s)
			in pickler pu v (s,pe)
			end,
	    fn (s,upe) =>
	    let val (w,s) = S.getcw s
		val w = w32_to_w w
	    in if w = NOT_THERE then unpickler pu (s,upe)
	       else let val v = lookup w
		    in (v,(s,upe))
		    end
	    end,
	    hasher pu,
	    #4 pu)	   
	end
  end

