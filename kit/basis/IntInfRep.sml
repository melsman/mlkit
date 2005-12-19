(* Internal representation of IntInf.int including conversion
   functions to be used in the Int/IntN/Word/WordN
   implementations. This signature, as well as its matching structure
   is declared before any of the Int/IntN/Word/WordN modules. mael
   2005-12-14 *)

structure IntInfRep : INT_INF_REP =
  struct
      (* Some primitive conversions *)
      fun cast_wi (a: word) : int = prim("id", a)
      fun cast_iw (a: int) : word = prim("id", a)

      fun i32_i (x: int32) : int = prim("__int32_to_int", x)
      fun i_i32 (x: int) : int32 = prim("__int_to_int32", x)
      fun i31_i32 (x: int31) : int32 = prim("__int31_to_int32", x)
      fun i32_i31 (x: int32) : int31 = prim("__int32_to_int31", x)
      fun i31_i (x: int31) : int = prim("__int31_to_int", x)
      fun i_i31 (x: int) : int31 = prim("__int_to_int31", x)
      fun w32_w (w : word32) : word = cast_iw(prim("__word32_to_int", w))
      fun w32_w_X (w : word32) : int = prim("__word32_to_int_X", w)
      fun w_w32 (w : word) : word32 = prim("__word_to_word32", w)
      fun w_w32_X (w : word) : word32 = prim("__word_to_word32_X", w)
      fun w31_w32 (w : word31) : word32 = prim("__word31_to_word32", w)
      fun w31_w32_X (w : word31) : word32 = prim("__word31_to_word32_X", w)
      fun w32_w31 (w : word32) : word31 = prim("__word32_to_word31", w)
      
      fun w_i_X (w : word) : int = prim("id", w)
      fun w_i (w : word) : int = 
	  let val i = w_i_X w
	  in if i < 0 then raise Overflow else i
	  end
      fun i_w (i : int) : word = prim("id", i)

      fun w31_i (w : word31) : int = w_i(prim("__word31_to_word", w))
      fun w31_i_X (w : word31) : int = cast_wi(prim("__word31_to_word_X", w))
      fun i_w31 (i : int) : word31 = prim("__word_to_word31", cast_iw i)
      fun i_w32 (i : int) : word32 = prim("__word_to_word32", cast_iw i)
      fun w32_i32 (w : word32) : int32 = prim("__word32_to_int32", w)
      fun w32_i32_X (w : word32) : int32 = prim("__word32_to_int32_X", w)
      fun i32_w32 (i : int32) : word32 = prim("__int32_to_word32", i)
      fun i31_w32 (i : int31) : word32 = i32_w32(i31_i32 i)
      fun i31_w (i : int31) : word = w32_w(i32_w32(i31_i32 i))
      fun i31_w31 (i : int31) : word31 = prim("id", i)
      fun w31_i31 (i : word31) : int31 = prim("id", i)


      fun rshiftW32 (w : word32, k : word) : word32 = 
	  if k >= 0w32 then 0w0
	  else prim("__shift_right_unsigned_word32", (w,k))
      fun andbW32 (x : word32, y : word32) : word32 = prim("__andb_word32", (x, y))
      fun andbW31 (x : word31, y : word31) : word31 = prim("__andb_word31", (x, y))
      fun andbW (x : word, y : word) : word = prim("__andb_word", (x, y))

      local
	  fun w_w8 (w : word) : word8 = prim("id", w)
	  fun norm (w: word) : word = andbW (0w255, w)
      in
	  fun w8_w (w : word8) : word = prim("id", w)
	  fun w32_w8 (w: word32) : word8 = w_w8(norm(prim ("__word32_to_word", w)))
	  fun w8_w32 (w: word8) : word32 = w_w32(w8_w w)
      end

      fun quoti31(x:int31,y:int31) : int31 = 
	  if y = 0 then raise Div
	  else prim ("__quot_int31", (x, y)) 

      fun lshiftw31 (w : word31, k) : word31 =
	  if k >= cast_iw 31 then 0w0
	  else prim("__shift_left_word31", (w,k))

      fun rshiftw31X (w : word31, k) : word31 = 
	if k >= cast_iw 31 then 
	  if cast_wi(prim("__word31_to_word_X", w)) >= 0 then 0w0 (* msbit = 0 *)
	  else prim("__word_to_word31", cast_iw ~1)  (* msbit = 1 *)
	else prim("__shift_right_signed_word31", (w,k))
	  
      (* BigNat operations - IntInf is based on bignat = int31 list *)
      type bignat = int31 list
      structure BN =
	  struct		      
	      (* digits are 31 bits *)
	      val nbase : int31 = ~0x40000000     (* = ~2^lgBase *)
	      val nbaseI32 : int32 = ~0x40000000
	      val nbaseW32Not : word32 = 0wx3fffffff 
	      val lgBase : int31 = 30             (* No. of bits per digit; must be even *)	 
	      val lgBaseW : word = 0w30
	      fun maxDigit() : int31 = 1073741823
	      val maxDigitI32 : int32 = 1073741823

	      fun lgHBase() : int31 = quoti31 (lgBase, 2)    (* half digits *)
	      fun hbase() : word31 = lshiftw31(0w1, i31_w (lgHBase()))
	      fun hmask() : word31 = (hbase())-0w1

	      fun zero() : bignat = []

	      fun hl i = 
		  let val w = i31_w31 i
		  in (w31_i31(rshiftw31X(w, i31_w (lgHBase()))),  (* MUST sign-extend *)
		      w31_i31(andbW31(w, hmask())))
		  end

	      fun sh i = w31_i31(lshiftw31 (i31_w31 i, i31_w (lgHBase())))

	      fun addOne [] : bignat = [1]
		| addOne (m::rm) = 
		  let val c = nbase+m+1
		  in if c < 0 then (c-nbase)::rm 
		     else c::(addOne rm)
		  end
	      fun add ([], digits) : bignat = digits
		| add (digits, []) = digits
		| add (dm::rm, dn::rn) = addd (nbase+dm+dn, rm, rn)
	      and addd (s, m, n) : bignat = 
		  if s < 0 then (s-nbase) :: add (m, n) 
		  else (s :: addc (m, n))
	      and addc (m, []) : bignat = addOne m
		| addc ([], n) = addOne n
		| addc (dm::rm, dn::rn) = addd (nbase+dm+dn+1, rm, rn)
	      exception Negative
	      fun consd (0:int31, []) = []
		| consd (d, r) = d::r
	      fun subt (m, []) : bignat = m
		| subt ([], n) = raise Negative
		| subt (dm::rm, dn::rn) = subd(dm-dn,rm,rn)
	      and subb ([], n) = raise Negative
		| subb (dm::rm, []) = subd (dm-1, rm, [])
		| subb (dm::rm, dn::rn) = subd (dm-dn-1, rm, rn)
	      and subd (d:int31, m:bignat, n:bignat) = 
		  if d >= 0 then consd(d, subt (m, n)) 
		  else consd(d-nbase, subb (m, n))

	      (* multiply 2 digits *)
	      fun mul2 (m:int31, n:int31) = 
		  let val (mh, ml) = hl m
		      val (nh, nl) = hl n
		      val x = mh*nh
		      val y = (mh-ml)*(nh-nl) (* x-y+z = mh*nl + ml*nh *)
		      val z = ml*nl
		      val (zh, zl) = hl z
		      val (uh,ul) = hl (nbase+x+z-y+zh) (* can't overflow *)
		  in (x+uh+ w31_i31 (hbase()), sh ul+zl) 
		  end
	      
	      (* multiply bigint by digit *)
	      fun muld (m, 0) = []
		| muld (m, 1) = m (* speedup *)
		| muld (m, i) = 
		  let fun muldc ([], 0) = []
			| muldc ([], c) = [c]
			| muldc (d::r, c) = 
		         let val (h, l) = mul2 (d, i)
			     val l1 = l+nbase+c
			 in if l1 >= 0 then l1::muldc (r, h+1)
			    else (l1-nbase)::muldc (r, h) 
			 end
		  in muldc (m, 0) 
		  end

	      fun mult (m, []) = []
		| mult (m, [d]) = muld (m, d) (* speedup *)
		| mult (m, 0::r) = consd (0, mult (m, r)) (* speedup *)
		| mult (m, n) = 
		  let fun muln [] = []
			| muln (d::r) = add (muld (n, d), consd (0, muln r))
		  in muln m 
		  end	      
	  end

      (* the IntInf datatype *)	  
      datatype intinf = datatype intinf 
      (* = IntInf of {negative : bool, digits : int31 list} *)

      local
	  fun minNeg() : int32 = ~2147483648 
	  fun maxDigit() : int32 = 1073741823  (* int31 *)
	  fun nbase() = BN.nbaseI32
	  fun lgBase() = BN.lgBaseW
	  val notNbase = BN.nbaseW32Not
	  fun natInfFromI32 (0 : int32) : int31 list = []
	    | natInfFromI32 i = 
	      let
		fun bn (0w0 : word32) = []
		  | bn i = 
		    let	fun dmbase n = (rshiftW32(n, lgBase()), andbW32 (n, notNbase))
			val (q,r) = dmbase i
		    in (i32_i31(w32_i32 r)) :: bn q
		    end
	    in if i <= maxDigit() then [i32_i31 i] 
	       else bn (i32_w32 i)
	    end

	fun natInfToI32 [] : int32 = 0
	  | natInfToI32 [d] = i31_i32 d
	  | natInfToI32 [d,e] = ~(nbase()*(i31_i32 e)) + i31_i32 d
	  | natInfToI32 (d::r) = ~(nbase()*natInfToI32 r) + i31_i32 d

	fun bigNatMinNeg() = BN.addOne (natInfFromI32 (~(minNeg()+1)))
	fun negi digits = IntInf{negative=true, digits=digits}
	fun bigIntMinNeg() = negi (bigNatMinNeg())

	fun intInfToI32 (IntInf{digits=[], ...}) = 0
	  | intInfToI32 (IntInf{negative=false, digits}) = natInfToI32 digits
	  | intInfToI32 (IntInf{negative=true, digits}) =
	    (~(natInfToI32 digits)) handle _ =>
                if digits = bigNatMinNeg() then minNeg() 
		else raise Overflow

	fun zero() = IntInf{negative=false, digits=BN.zero()}
	fun i32ToIntInf (0:int32) = zero()
	  | i32ToIntInf i =
	    if i < 0 then 
		if (i = minNeg()) then bigIntMinNeg()
		else IntInf{negative=true, digits= natInfFromI32 (~i)}
            else IntInf{negative=false, digits= natInfFromI32 i}
      in
	  val zero = zero
	  fun toInt31 x = i32_i31(intInfToI32 x)
	  fun toInt32 x = intInfToI32 x
	  fun toInt x = i32_i(intInfToI32 x)
	      
	  fun fromInt x = i32ToIntInf(i_i32 x)
	  fun fromInt32 x = i32ToIntInf x
	  fun fromInt31 x = i32ToIntInf (i31_i32 x)
      end

      fun subtNat (m, []) = {negative=false, digits=m}
	| subtNat ([], n) = {negative=true, digits=n}
	| subtNat (m,n) =
	  ({negative=false,digits = BN.subt(m,n)})
	  handle BN.Negative => ({negative=true,digits = BN.subt(n,m)})

      fun IntInfPlus (IntInf{digits=[], ...}, i2) = i2
	| IntInfPlus (i1, IntInf{digits=[], ...}) = i1
	| IntInfPlus (IntInf{negative=false, digits=d1}, IntInf{negative=true, digits=d2}) =
          IntInf(subtNat(d1, d2))
	| IntInfPlus (IntInf{negative=true, digits=d1}, IntInf{negative=false, digits=d2}) =
          IntInf(subtNat(d2, d1))
	| IntInfPlus (IntInf{negative, digits=d1}, IntInf{digits=d2, ...}) =
          IntInf{negative=negative, digits=BN.add(d1, d2)}

      fun negSign false = true
	| negSign true = false
	  
      fun neg (i as IntInf{digits=[], ...}) = i
	| neg (IntInf{negative=false, digits}) = IntInf{negative=true, digits=digits}
	| neg (IntInf{negative=true, digits}) = IntInf{negative=false, digits=digits}

      val maxInt32W : word32 = 0w2147483647 
      fun fromWord32 (w : word32) : intinf =	  
	  if w <= maxInt32W then fromInt32 (w32_i32 w)
	  else 
	      let val w2 = fromInt32(w32_i32 (w div 0w3))
		  val rest = fromInt32(w32_i32 (w mod 0w3))
		  val op + = IntInfPlus
	      in w2 + w2 + w2 + rest
	      end

      fun fromWord32X (w:word32) : intinf =
	  fromInt32(w32_i32_X w)

      fun fromWord31X (w:word31) : intinf =
	  fromWord32X(w31_w32_X w)

      fun fromWordX (w:word) : intinf =
	  fromWord32X(w_w32_X w)

      fun toWord32 (x : intinf) : word32 =
	  i32_w32(toInt32 x) 
	  handle _ => raise Fail "IntInfRep.toWord32"

      fun fromWord (w : word) : intinf =
	  fromWord32 (w_w32 w)

      fun toWord (x : intinf) : word =
	  w32_w(toWord32 x)

      fun fromWord8 (w8 : word8) : intinf =
	  fromWord32 (w8_w32 w8)

      fun fromWord8X (w:word8) : intinf =
	  if w < 0w128 then fromWord8 w
	  else neg(fromWord8 (0w255 - w + 0w1))

      fun toWord8 (x : intinf) : word8 =
	  w32_w8(toWord32 x)
	  
      fun fromWord31 (w31 : word31) : intinf =
	  fromWord32 (w31_w32 w31)

      fun toWord31 (x : intinf) : word31 =
	  w32_w31(toWord32 x)

      (* for overloading *)
      val ~ = neg

      fun op * (_,IntInf{digits=[], ...}) = zero()
	| op * (IntInf{digits=[], ...},_) = zero()
	| op * (IntInf{negative=false, digits=d1}, IntInf{negative=true, digits=d2}) =
          IntInf{negative=true,digits=BN.mult(d1,d2)}
	| op * (IntInf{negative=true, digits=d1}, IntInf{negative=false, digits=d2}) =
          IntInf{negative=true,digits=BN.mult(d1,d2)}
	| op * (IntInf{digits=d1,...}, IntInf{digits=d2,...}) =
          IntInf{negative=false,digits=BN.mult(d1,d2)}

      val op + = IntInfPlus

      fun op - (i1, IntInf{digits=[], ...}) = i1
	| op - (IntInf{digits=[], ...}, IntInf{negative, digits}) =
          IntInf{negative=negSign negative, digits=digits}
	| op - (IntInf{negative=false, digits=d1}, IntInf{negative=false, digits=d2}) =
	  IntInf(subtNat(d1, d2))
	| op - (IntInf{negative=true, digits=d1}, IntInf{negative=true, digits=d2}) =
	  IntInf(subtNat(d2, d1))
	| op - (IntInf{negative, digits=d1}, IntInf{digits=d2, ...}) =
          IntInf{negative=negative, digits=BN.add(d1, d2)}

(*
      val div : intinf * intinf -> intinf
      val mod : intinf * intinf -> intinf
      val abs : intinf -> intinf
      val <   : intinf * intinf -> intinf
      val >   : intinf * intinf -> intinf
      val <=  : intinf * intinf -> intinf
      val >=  : intinf * intinf -> intinf
*)
  end