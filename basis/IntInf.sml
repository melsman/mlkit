(* IntInf.sml
 *
 * Added divMod, quotRem
 * Implementation originates from SML/NJ;  mael 2001-10-31
 * eta-convertion for dead-code elimination.. mael 2001-12-12
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories. See COPYRIGHT file for details.
 *
 * This package is derived from Andrzej Filinski's bignum package.  Eventually,
 * it should be moved to the basis.
 *
 * It is implemented almost totally on the abstraction presented by
 * the BigNat structure. The only concrete type information it assumes
 * is that BigNat.bignat = 'a list and that BigNat.zero = [].
 * Some trivial additional efficiency could be obtained by assuming that
 * type bignat is really int list, and that if (v : bignat) = [d], then
 * bignat d = [d].
 *
 * At some point, this should be reimplemented to make use of Word32, or
 * have compiler/runtime support.
 *
 * Also, for booting, this module could be broken into one that has
 * all the types and arithmetic functions, but doesn't use NumScan,
 * constructing values from strings using bignum arithmetic. Various
 * integer and word scanning, such as NumScan, could then be constructed
 * from IntInf. Finally, a user-level IntInf could be built by
 * importing the basic IntInf, but replacing the scanning functions
 * by more efficient ones based on the functions in NumScan.
 *
 *)

(** SigDoc *)
structure IntInf : INT_INF =
  struct

  (* It is not clear what advantage there is to having NumFormat as
   * a submodule.
   *)

    structure NumScan : sig

        val skipWS : (char, 'a) StringCvt.reader -> 'a -> 'a

        val scanInt : StringCvt.radix
	      ->  (char, 'a) StringCvt.reader
	        -> 'a -> (int31 * 'a) option
	    (** should be to int32 **)

      end = struct

        structure W = Word32
        structure I = Int31

        val op <  = W.<
        val op >= = W.>=
        val op +  = W.+
        val op -  = W.-
        val op *  = W.*

        val largestWordDiv10 : Word32.word = 0w429496729(* 2^32-1 divided by 10 *)
        val largestWordMod10 : Word32.word = 0w5	(* remainder *)
        val largestNegInt : Word32.word = 0w1073741824	(* absolute value of ~2^30 *)
        val largestPosInt : Word32.word = 0w1073741823	(* 2^30-1 *)

        type 'a chr_strm = {getc : (char, 'a) StringCvt.reader}

      (* A table for mapping digits to values.  Whitespace characters map to
       * 128, "+" maps to 129, "-","~" map to 130, "." maps to 131, and the
       * characters 0-9,A-Z,a-z map to their * base-36 value.  All other
       * characters map to 255.
       *)
        local
          val cvtTable = "\
    	    \\255\255\255\255\255\255\255\255\255\128\128\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\128\255\255\255\255\255\255\255\255\255\255\129\255\130\131\255\
    	    \\000\001\002\003\004\005\006\007\008\009\255\255\255\255\255\255\
    	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
    	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\255\255\
    	    \\255\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\
    	    \\025\026\027\028\029\030\031\032\033\034\035\255\255\255\130\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	    \\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    	  \"
        val ord = Char.ord
        in
	fun code (c : char) = W.fromInt(ord(CharVector.sub(cvtTable, ord c)))
        val wsCode : Word32.word = 0w128
        val plusCode : Word32.word = 0w129
        val minusCode : Word32.word = 0w130
        end (* local *)

        fun skipWS (getc : (char, 'a) StringCvt.reader) cs = let
              fun skip cs = (case (getc cs)
		     of NONE => cs
		      | (SOME(c, cs')) => if (code c = wsCode) then skip cs' else cs
		    (* end case *))
              in
                skip cs
              end

      (* skip leading whitespace and any sign (+, -, or ~) *)
        fun scanPrefix (getc : (char, 'a) StringCvt.reader) cs = let
    	  fun noSkipWS cs = (case (getc cs)
    		 of NONE => NONE
    		  | (SOME(c, cs')) => let val c' = code c
    		      in
    			(*if (c' = wsCode) then skipWS cs' else*) SOME(c', cs')   (*mael 2005-12-13*)
    		      end
    		(* end case *))
    	  fun getNext (neg, cs) = (case (getc cs)
    		 of NONE => NONE
    		  | (SOME(c, cs)) => SOME{neg=neg, next=code c, rest=cs}
    		(* end case *))
    	  in
    	    case (noSkipWS cs)
    	     of NONE => NONE
    	      | (SOME(c, cs')) =>
    		  if (c = plusCode) then getNext(false, cs')
    		  else if (c = minusCode) then getNext(true, cs')
    		  else SOME{neg=false, next=c, rest=cs'}
    	    (* end case *)
    	  end

      (* for power of 2 bases (2, 8 & 16), we can check for overflow by looking
       * at the hi (1, 3 or 4) bits.
       *)
        fun chkOverflow mask w =
    	  if (W.andb(mask, w) = 0w0) then () else raise Overflow

        fun scanBin (getc : (char, 'a) StringCvt.reader) cs = (case (scanPrefix getc cs)
    	   of NONE => NONE
    	    | (SOME{neg, next, rest}) => let
    		fun isDigit (d : Word32.word) = (d < 0w2)
    		val chkOverflow = chkOverflow 0wx80000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME{neg=neg, word=w, rest=rest}
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w1), d), rest'))
    				else SOME{neg=neg, word=w, rest=rest}
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))

        fun scanOct getc cs = (case (scanPrefix getc cs)
    	   of NONE => NONE
    	    | (SOME{neg, next, rest}) => let
    		fun isDigit (d : Word32.word) = (d < 0w8)
    		val chkOverflow = chkOverflow 0wxE0000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME{neg=neg, word=w, rest=rest}
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w3), d), rest'))
    				else SOME{neg=neg, word=w, rest=rest}
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))

        fun scanDec getc cs = (case (scanPrefix getc cs)
    	   of NONE => NONE
    	    | (SOME{neg, next, rest}) => let
    		fun isDigit (d : Word32.word) = (d < 0w10)
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME{neg=neg, word=w, rest=rest}
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  if ((w >= largestWordDiv10)
    				  andalso ((largestWordDiv10 < w)
    				    orelse (largestWordMod10 < d)))
    				    then raise Overflow
    				    else ();
    				  cvt (0w10*w+d, rest'))
    				else SOME{neg=neg, word=w, rest=rest}
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))

        fun scanHex getc cs = (case (scanPrefix getc cs)
    	   of NONE => NONE
    	    | (SOME{neg, next, rest}) => let
    		fun isDigit (d : Word32.word) = (d < 0w16)
    		val chkOverflow = chkOverflow 0wxF0000000
    		fun cvt (w, rest) = (case (getc rest)
    		       of NONE => SOME{neg=neg, word=w, rest=rest}
    			| SOME(c, rest') => let val d = code c
    			    in
    			      if (isDigit d)
    				then (
    				  chkOverflow w;
    				  cvt(W.+(W.<<(w, 0w4), d), rest'))
    				else SOME{neg=neg, word=w, rest=rest}
    			    end
    		      (* end case *))
    		in
    		  if (isDigit next)
    		    then cvt(next, rest)
    		    else NONE
    		end
    	  (* end case *))

        fun finalInt scanFn getc cs = (case (scanFn getc cs)
    	   of NONE => NONE
    	    | (SOME{neg=true, word, rest}) =>
    		if (largestNegInt < word)
    		  then raise Overflow
    		  else SOME(I.~(I.fromInt(W.toInt word)), rest)
    	    | (SOME{word, rest, ...}) =>
    		if (largestPosInt < word)
    		  then raise Overflow
    		  else SOME(I.fromInt(W.toInt word), rest)
    	  (* end case *))

        fun scanInt StringCvt.BIN = finalInt scanBin
          | scanInt StringCvt.OCT = finalInt scanOct
          | scanInt StringCvt.DEC = finalInt scanDec
          | scanInt StringCvt.HEX = finalInt scanHex

      end (* structure NumScan *)

    structure NumFormat : sig

        val fmtWord : StringCvt.radix -> Word32.word -> string
        val fmtInt : StringCvt.radix -> int31 -> string	(** should be int32 **)

      end = struct

        structure W = Word32
        structure I = Int31

        val op < = W.<
        val op - = W.-
        val op * = W.*
        val op div = W.div

        fun mkDigit (w : Word32.word) =
    	  CharVector.sub("0123456789abcdef", W.toInt w)

        fun wordToBin w = let
    	  fun mkBit w = if (W.andb(w, 0w1) = 0w0) then #"0" else #"1"
    	  fun f (0w0, n, l) = (I.+(n, 1), #"0" :: l)
    	    | f (0w1, n, l) = (I.+(n, 1), #"1" :: l)
    	    | f (w, n, l) = f(W.>>(w, 0w1), I.+(n, 1), (mkBit w) :: l)
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToOct w = let
    	  fun f (w, n, l) = if (w < 0w8)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else f(W.>>(w, 0w3), I.+(n, 1), mkDigit(W.andb(w, 0w7)) :: l)
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToDec w = let
    	  fun f (w, n, l) = if (w < 0w10)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else let val j = w div 0w10
    		  in
    		    f (j,  I.+(n, 1), mkDigit(w - 0w10*j) :: l)
    		  end
    	  in
    	    f (w, 0, [])
    	  end
        fun wordToHex w = let
    	  fun f (w, n, l) = if (w < 0w16)
    		then (I.+(n, 1), (mkDigit w) :: l)
    		else f(W.>>(w, 0w4), I.+(n, 1), mkDigit(W.andb(w, 0w15)) :: l)
    	  in
    	    f (w, 0, [])
    	  end

        fun fmtW StringCvt.BIN = #2 o wordToBin
          | fmtW StringCvt.OCT = #2 o wordToOct
          | fmtW StringCvt.DEC = #2 o wordToDec
          | fmtW StringCvt.HEX = #2 o wordToHex

        fun fmtWord radix = String.implode o (fmtW radix)

    (** NOTE: this currently uses 31-bit integers, but really should use 32-bit
     ** ints (once they are supported).
     **)
        fun fmtInt radix = let
    	  val fmtW = fmtW radix
    	  val itow = W.fromInt o Int31.toInt
    	  fun fmt i = if I.<(i, 0)
    		then let
    		  val (digits) = fmtW(itow(I.~ i))
    		  in
    		    String.implode(#"~"::digits)
    		  end
    		    handle _ => (case radix
    		       of StringCvt.BIN => "~1111111111111111111111111111111"
    			| StringCvt.OCT => "~7777777777"
    			| StringCvt.DEC => "~1073741824"
    			| StringCvt.HEX => "~3fffffff"
    		      (* end case *))
    		else String.implode(fmtW(itow i))
    	  in
    	    fmt
    	  end

      end (* structure NumFormat *)

    structure BigNat =
      struct

	exception Negative

        val itow : int31 -> word31 = Word31.fromInt o Int31.toInt
        val itow' : int31 -> word = Word.fromInt o Int31.toInt
	val wtoi : word31 -> int31 = Int31.fromInt o Word31.toIntX

	val lgBase : int31 = 30             (* No. of bits per digit; must be even *)
	val nbase : int31 = ~0x40000000     (* = ~2^lgBase *)

	fun maxDigit() : int31 = Int31.~(nbase + 1)

	fun lgHBase() : int31 = Int31.quot (lgBase, 2)    (* half digits *)
	fun hbase() : word31 = Word31.<<(0w1, itow' (lgHBase()))
	fun hmask() : word31 = (hbase())-0w1

	fun quotrem (i, j) = (Int31.quot (i, j), Int31.rem (i, j))
	fun scale i : int31 = if i = maxDigit() then 1 else Int31.div(nbase, Int31.~(i+1))

	type bignat = int31 list (* least significant digit first *)

	fun zero() : bignat = []
	fun one() : bignat = [1]

	fun bignat 0 = zero()
	  | bignat i = let
	      val notNbase = Word31.notb(itow nbase)
              fun bn 0w0 = []
        	| bn i = let
		    fun dmbase n =
		      (Word31.>> (n, itow' lgBase), Word31.andb (n, notNbase))
		    val (q,r) = dmbase i
		  in
		    (wtoi r)::(bn q)
		  end
              in
        	if i > 0
        	  then if i <= maxDigit() then [i] else bn (itow i)
        	  else raise Negative
              end

	fun int [] = 0
	  | int [d] = d
	  | int [d,e] = ~(nbase*e) + d
	  | int (d::r) = ~(nbase*int r) + d

	fun consd (0:int31, []) = []
	  | consd (d, r) = d::r

	fun hl i = let
	  val w = itow i
        in
	  (wtoi(Word31.~>> (w, itow' (lgHBase()))),  (* MUST sign-extend *)
	   wtoi(Word31.andb(w, hmask())))
        end

	fun sh i = wtoi(Word31.<< (itow i, itow' (lgHBase())))

	fun addOne [] : bignat = [1]
	  | addOne (m::rm) = let
              val c = nbase+m+1
              in
        	if c < 0 then (c-nbase)::rm else c::(addOne rm)
              end

	fun add ([], digits) : bignat = digits
	  | add (digits, []) = digits
	  | add (dm::rm, dn::rn) = addd (nbase+dm+dn, rm, rn)
	and addd (s, m, n) : bignat =
              if s < 0 then (s-nbase) :: add (m, n) else (s :: addc (m, n))
	and addc (m, []) : bignat = addOne m
	  | addc ([], n) = addOne n
	  | addc (dm::rm, dn::rn) = addd (nbase+dm+dn+1, rm, rn)

        exception IntInf_subtOne

	fun subtOne (0::mr) : bignat = maxDigit()::(subtOne mr)
	  | subtOne [1] = []
	  | subtOne (n::mr) = (n-1)::mr
	  | subtOne [] = raise IntInf_subtOne

	fun subt (m, []) : bignat = m
	  | subt ([], n) = raise Negative
	  | subt (dm::rm, dn::rn) = subd(dm-dn,rm,rn)
	and subb ([], n) = raise Negative
	  | subb (dm::rm, []) = subd (dm-1, rm, [])
	  | subb (dm::rm, dn::rn) = subd (dm-dn-1, rm, rn)
	and subd (d:int31, m:bignat, n:bignat) =
              if d >= 0 then consd(d, subt (m, n)) else consd(d-nbase, subb (m, n))

	(* multiply 2 digits *)
	fun mul2 (m:int31, n:int31) = let
              val (mh, ml) = hl m
              val (nh, nl) = hl n
              val x = mh*nh
              val y = (mh-ml)*(nh-nl) (* x-y+z = mh*nl + ml*nh *)
              val z = ml*nl
              val (zh, zl) = hl z
              val (uh,ul) = hl (nbase+x+z-y+zh) (* can't overflow *)
              in (x+uh+wtoi (hbase()), sh ul+zl) end

        (* multiply bigint by digit *)
	fun muld (m, 0) = []
	  | muld (m, 1) = m (* speedup *)
	  | muld (m, i) = let
              fun muldc ([], 0) = []
        	| muldc ([], c) = [c]
        	| muldc (d::r, c) = let
                    val (h, l) = mul2 (d, i)
                    val l1 = l+nbase+c
                    in
                      if l1 >= 0
                	then l1::muldc (r, h+1)
                	else (l1-nbase)::muldc (r, h)
                    end
              in muldc (m, 0) end

	fun mult (m, []) = []
	  | mult (m, [d]) = muld (m, d) (* speedup *)
	  | mult (m, 0::r) = consd (0, mult (m, r)) (* speedup *)
	  | mult (m, n) = let
              fun muln [] = []
        	| muln (d::r) = add (muld (n, d), consd (0, muln r))
              in muln m end

        (* divide DP number by digit; assumes u < i , i >= base/2 *)
	fun divmod2 ((u,v), i) = let
              val (vh,vl) = hl v
              val (ih,il) = hl i
              fun adj (q,r) = if r<0 then adj (q-1, r+i) else (q, r)
              val (q1,r1) = quotrem (u, ih)
              val (q1,r1) = adj (q1, sh r1+vh-q1*il)
              val (q0,r0) = quotrem (r1, ih)
              val (q0,r0) = adj (q0, sh r0+vl-q0*il)
              in (sh q1+q0, r0) end

        exception IntInf_divmodd

        (* divide bignat by digit>0 *)
	fun divmodd (m, 1) = (m, 0:int31) (* speedup *)
	  | divmodd (m, i) = let
              val scale = scale i
              val i' = i * scale
              val m' = muld (m, scale)
              fun dmi [] = ([], 0)
        	| dmi (d::r) = let
                    val (qt,rm) = dmi r
                    val (q1,r1) = divmod2 ((rm,d), i')
                    in (consd (q1,qt), r1) end
              val (q,r) = dmi m'
	      val _ = if scale = 0 then raise IntInf_divmodd else ()
              in (q, Int31.div(r,scale)) end

        (* From Knuth Vol II, 4.3.1, but without opt. in step D3 *)
	fun divmod (m, []) = raise Div
	  | divmod ([], n) = ([], []) (* speedup *)
	  | divmod (d::r, 0::s) = let
              val (qt,rm) = divmod (r,s)
              in (qt, consd (d, rm)) end (* speedup *)
	  | divmod (m, [d]) = let
              val (qt, rm) = divmodd (m, d)
              in (qt, if rm=0 then [] else [rm]) end
	  | divmod (m, n) = let
              val ln = length n (* >= 2 *)
              val scale = scale(List.nth (n,ln-1))
              val m' = muld (m, scale)
              val n' = muld (n, scale)
              val n1 = List.nth (n', ln-1) (* >= base/2 *)
              fun divl [] = ([], [])
        	| divl (d::r) = let
                    val (qt,rm) = divl r
                    val m = consd (d, rm)
                    fun msds ([],_) = (0,0)
                      | msds ([d],1) = (0,d)
                      | msds ([d2,d1],1) = (d1,d2)
                      | msds (d::r,i) = msds (r,i-1)
                    val (m1,m2) = msds (m, ln)
                    val tq = if m1 = n1 then maxDigit()
                             else #1 (divmod2 ((m1,m2), n1))
                    fun try (q,qn') = (q, subt (m,qn'))
                	  handle Negative => try (q-1, subt (qn', n'))
                    val (q,rr) = try (tq, muld (n',tq))
                    in (consd (q,qt), rr) end
              val (qt,rm') = divl m'
              val (rm,_(*0*)) = divmodd (rm',scale)
              in (qt,rm) end

	fun cmp ([],[]) = EQUAL
	  | cmp (_,[]) = GREATER
	  | cmp ([],_) = LESS
	  | cmp ((i : int31)::ri,j::rj) =
              case cmp (ri,rj) of
        	EQUAL => if i = j then EQUAL
                         else if i < j then LESS
                         else GREATER
              | c => c

	fun exp (_, 0) = one()
	  | exp ([], n) = if n > 0 then zero() else raise Div
	  | exp (m, n) =
              if n < 0 then zero()
              else let
        	fun expm 0 = [1]
        	  | expm 1 = m
        	  | expm i = let
                      val r = expm (i div 2)
                      val r2 = mult (r,r)
                      in
                	if i mod 2 = 0 then r2 else mult (r2, m)
                      end
        	in expm n end

        local
          fun try n = if n >= lgHBase() then n else try (2*n)
          fun pow2lgHBase() = try 1
        in
        fun log2 [] = raise Domain
          | log2 (h::t) = let
              fun qlog (x,0) = 0
                | qlog (x,b) =
		  if x >= wtoi(Word31.<< (0w1, itow' b)) then
		    b+qlog (wtoi(Word31.>> (itow x, itow' b)), b div 2)
                                 else qlog (x, b div 2)
              fun loop (d,[],lg) = lg + qlog (d,pow2lgHBase())
                | loop (_,h::t,lg) = loop (h,t,lg + lgBase)
            in
              loop (h,t,0)
            end
        end (* local *)

            (* find maximal maxpow s.t. radix^maxpow < base
             * basepow = radix^maxpow
             *)
        fun mkPowers radix = let
	      val powers = let
                    val bnd = Int31.quot (nbase, (~radix))
                    fun try (tp,l) =
                          (if tp <= bnd then try (radix*tp,tp::l)
                          else (tp::l))
                            handle _ => tp::l
                    in Vector.fromList(rev(try (radix,[1]))) end
	      val maxpow = Vector.length powers - 1
              in
                (maxpow, Vector.sub(powers,maxpow), powers)
              end
        fun powers2() = mkPowers 2
        fun powers8() = mkPowers 8
        fun powers10() = mkPowers 10
        fun powers16() = mkPowers 16

        exception IntInf_fmt
	fun fmt (pow, radpow, puti) n = let
              val pad = StringCvt.padLeft #"0" pow
              fun ms0 (0,a) = (pad "")::a
        	| ms0 (i,a) = (pad (puti i))::a
              fun ml (n,a) =
		  if radpow < 0 then raise IntInf_fmt
		  else
                    case divmodd (n, radpow) of
                      ([],d) => (puti d)::a
                    | (q,d) => ml (q, ms0 (d, a))
              in
                concat (ml (n,[]))
              end

        fun fmt2() = fmt (#1 (powers2()), #2 (powers2()), NumFormat.fmtInt StringCvt.BIN)
        fun fmt8() = fmt (#1 (powers8()), #2 (powers8()), NumFormat.fmtInt StringCvt.OCT)
        fun fmt10() = fmt (#1 (powers10()), #2 (powers10()), NumFormat.fmtInt StringCvt.DEC)
        fun fmt16() = fmt (#1 (powers16()), #2 (powers16()), NumFormat.fmtInt StringCvt.HEX)

        type ('a,'b) reader = ('a,'b) StringCvt.reader

        fun scan (bound,powers,geti) (getc:(char,'s)reader) (cs:'s) : (bignat*'s) option =
            let
              fun get (l,cs) = if l = bound then NONE
                               else case getc cs of
                                 NONE => NONE
                               | SOME(c,cs') => SOME(c, (l+1,cs'))
              fun loop (acc,cs) =
                    case geti get (0,cs) of
                      NONE => (acc,cs)
                    | SOME(0,(sh,cs')) =>
                        loop(add(muld(acc,Vector.sub(powers,sh)),[]),cs')
                    | SOME(i,(sh,cs')) =>
                        loop(add(muld(acc,Vector.sub(powers,sh)),[i]),cs')
              in
                case geti get (0,cs) of
                  NONE => NONE
                | SOME(0,(_,cs')) => SOME (loop([],cs'))
                | SOME(i,(_,cs')) => SOME (loop([i],cs'))
              end

        fun scan2 getc = scan(#1 (powers2()), #3 (powers2()), NumScan.scanInt StringCvt.BIN) getc
        fun scan8 getc = scan(#1 (powers8()), #3 (powers8()), NumScan.scanInt StringCvt.OCT) getc
        fun scan10 getc = scan(#1 (powers10()), #3 (powers10()), NumScan.scanInt StringCvt.DEC) getc
        fun scan16 getc = scan(#1 (powers16()), #3 (powers16()), NumScan.scanInt StringCvt.HEX) getc

      end (* structure BigNat *)

    structure BN = BigNat

    type int = intinf (* =
      _IntInf of {
        digits : BN.bignat,
        negative : bool
      }
    *)

    fun zero() = _IntInf{negative=false, digits=BN.zero()}
    fun one() = _IntInf{negative=false, digits=BN.one()}
    fun minus_one() = _IntInf{negative=true, digits=BN.one()}
    fun posi digits = _IntInf{negative=false, digits=digits}
    fun negi digits = _IntInf{negative=true, digits=digits}
    fun zneg [] = zero()
      | zneg digits = _IntInf{negative=true, digits=digits}

(*
    local
	fun minNeg() : int31 = valOf Int31.minInt
	fun bigNatMinNeg() = BN.addOne (BN.bignat (~(minNeg()+1)))
	fun bigIntMinNeg() = negi (bigNatMinNeg())
    in

	fun toInt (IntInf{digits=[], ...}) : Int.int = 0
	  | toInt (IntInf{negative=false, digits}) = Int31.toInt(BN.int digits)
	  | toInt (IntInf{negative=true, digits}) =
	    (Int31.toInt(~(BN.int digits))) handle _ =>
                if digits = bigNatMinNeg() then Int31.toInt(minNeg()) else raise Overflow

	fun fromInt 0 = zero()
	  | fromInt i =
	    let val i = Int31.fromInt i
	    in
		if i < 0
		    then if (i = minNeg())
			     then bigIntMinNeg()
			 else IntInf{negative=true, digits= BN.bignat (~i)}
		else IntInf{negative=false, digits= BN.bignat i}
	    end
    end (* local *)
*)

      (* Implementation of toInt and fromInt. The implementation works
       * for both Int=Int31 and Int=Int32 ; mael 2005-12-14 *)

    local
	fun minNeg() = valOf Int32.minInt
	fun maxDigit() = Int32.fromInt(Int31.toInt((BN.maxDigit())))
	fun nbase() = Int32.fromInt(Int31.toInt BN.nbase)
	fun lgBase() = Word.fromInt(Int31.toInt BN.lgBase)
	fun notNbase() = Word32.notb(Word32.fromInt(Int31.toInt BN.nbase))
	fun natInfFromI32 (0 : Int32.int) : int31 list = []
	  | natInfFromI32 i =
	    let
		fun bn (0w0 : Word32.word) = []
		  | bn i =
		    let
			fun dmbase n = (Word32.>> (n, lgBase()), Word32.andb (n, notNbase()))
			val (q,r) = dmbase i
		    in
			(Int31.fromInt(Word32.toInt r)) :: bn q
		    end
	    in
		if i <= maxDigit() then [Int31.fromInt(Int32.toInt i)]
		else bn (Word32.fromLargeInt(Int32.toLarge i))
	    end

	fun natInfToI32 [] : int32 = 0
	  | natInfToI32 [d] = Int32.fromInt (Int31.toInt d)
	  | natInfToI32 [d,e] = ~(nbase()*(Int32.fromInt (Int31.toInt e))) + (Int32.fromInt (Int31.toInt d))
	  | natInfToI32 (d::r) = ~(nbase()*natInfToI32 r) + (Int32.fromInt (Int31.toInt d))

	fun bigNatMinNeg() = BN.addOne (natInfFromI32 (~(minNeg()+1)))
	fun bigIntMinNeg() = negi (bigNatMinNeg())

	fun intInfToI32 (_IntInf{digits=[], ...}) = 0
	  | intInfToI32 (_IntInf{negative=false, digits}) = natInfToI32 digits
	  | intInfToI32 (_IntInf{negative=true, digits}) =
	    (Int32.~(natInfToI32 digits)) handle _ =>
                if digits = bigNatMinNeg() then minNeg() else raise Overflow

	fun i32ToIntInf (0:Int32.int) = zero()
	  | i32ToIntInf i =
	    if i < 0
		then if (i = minNeg())
			 then bigIntMinNeg()
		     else _IntInf{negative=true, digits= natInfFromI32 (Int32.~i)}
            else _IntInf{negative=false, digits= natInfFromI32 i}
    in
	fun toInt x = Int32.toInt(intInfToI32 x)
	fun fromInt x = i32ToIntInf(Int32.fromInt x)
    end (* local *)

    fun toLarge (x:intinf) : intinf = x
    fun fromLarge (x:intinf) : intinf = x

    fun negSign false = true
      | negSign true = false

    fun subtNat (m, []) = {negative=false, digits=m}
      | subtNat ([], n) = {negative=true, digits=n}
      | subtNat (m,n) =
            ({negative=false,digits = BN.subt(m,n)})
              handle BN.Negative => ({negative=true,digits = BN.subt(n,m)})

    val precision = NONE
    val minInt = NONE
    val maxInt = NONE

    fun ~ (i as _IntInf{digits=[], ...}) = i
      | ~ (_IntInf{negative=false, digits}) = _IntInf{negative=true, digits=digits}
      | ~ (_IntInf{negative=true, digits}) = _IntInf{negative=false, digits=digits}

    fun op * (_,_IntInf{digits=[], ...}) = zero()
      | op * (_IntInf{digits=[], ...},_) = zero()
      | op * (_IntInf{negative=false, digits=d1}, _IntInf{negative=true, digits=d2}) =
          _IntInf{negative=true,digits=BN.mult(d1,d2)}
      | op * (_IntInf{negative=true, digits=d1}, _IntInf{negative=false, digits=d2}) =
          _IntInf{negative=true,digits=BN.mult(d1,d2)}
      | op * (_IntInf{digits=d1,...}, _IntInf{digits=d2,...}) =
          _IntInf{negative=false,digits=BN.mult(d1,d2)}

    fun op + (_IntInf{digits=[], ...}, i2) = i2
      | op + (i1, _IntInf{digits=[], ...}) = i1
      | op + (_IntInf{negative=false, digits=d1}, _IntInf{negative=true, digits=d2}) =
          _IntInf(subtNat(d1, d2))
      | op + (_IntInf{negative=true, digits=d1}, _IntInf{negative=false, digits=d2}) =
          _IntInf(subtNat(d2, d1))
      | op + (_IntInf{negative, digits=d1}, _IntInf{digits=d2, ...}) =
          _IntInf{negative=negative, digits=BN.add(d1, d2)}

    fun op - (i1, _IntInf{digits=[], ...}) = i1
      | op - (_IntInf{digits=[], ...}, _IntInf{negative, digits}) =
          _IntInf{negative=negSign negative, digits=digits}
      | op - (_IntInf{negative=false, digits=d1}, _IntInf{negative=false, digits=d2}) =
            _IntInf(subtNat(d1, d2))
      | op - (_IntInf{negative=true, digits=d1}, _IntInf{negative=true, digits=d2}) =
            _IntInf(subtNat(d2, d1))
      | op - (_IntInf{negative, digits=d1}, _IntInf{digits=d2, ...}) =
          _IntInf{negative=negative, digits=BN.add(d1, d2)}

    fun quotrem (_IntInf{negative=false,digits=m},_IntInf{negative=false,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, posi r))
      | quotrem (_IntInf{negative=false,digits=m},_IntInf{negative=true,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (zneg q, posi r))
      | quotrem (_IntInf{negative=true,digits=m},_IntInf{negative=false,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (zneg q, zneg r))
      | quotrem (_IntInf{negative=true,digits=m},_IntInf{negative=true,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, zneg r))

    fun divmod (_IntInf{negative=false,digits=m},_IntInf{negative=false,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, posi r))
      | divmod (_IntInf{negative=false,digits=[]},_IntInf{negative=true,digits=n}) = (zero(),zero())
      | divmod (_IntInf{negative=false,digits=m},_IntInf{negative=true,digits=n}) = let
          val (q,r) = BN.divmod (BN.subtOne m, n)
          in (negi(BN.addOne q), zneg(BN.subtOne(BN.subt(n,r)))) end
      | divmod (_IntInf{negative=true,digits=m},_IntInf{negative=false,digits=n}) = let
          val (q,r) = BN.divmod (BN.subtOne m, n)
          in (negi(BN.addOne q), posi(BN.subtOne(BN.subt(n,r)))) end
      | divmod (_IntInf{negative=true,digits=m},_IntInf{negative=true,digits=n}) =
          (case BN.divmod (m,n) of (q,r) => (posi q, zneg r))

    fun op div arg = #1(divmod arg)
    fun op mod arg = #2(divmod arg)
    fun op quot arg = #1(quotrem arg)
    fun op rem arg = #2(quotrem arg)

    fun compare (_IntInf{negative=true,...},_IntInf{negative=false,...}) = LESS
      | compare (_IntInf{negative=false,...},_IntInf{negative=true,...}) = GREATER
      | compare (_IntInf{negative=false,digits=d},_IntInf{negative=false,digits=d'}) = BN.cmp (d,d')
      | compare (_IntInf{negative=true,digits=d},_IntInf{negative=true,digits=d'}) = BN.cmp (d',d)

    fun op < arg = case compare arg of LESS => true | _ => false
    fun op > arg = case compare arg of GREATER => true | _ => false
    fun op <= arg = case compare arg of GREATER => false | _ => true
    fun op >= arg = case compare arg of LESS => false | _ => true

    fun abs (_IntInf{negative=true, digits}) = _IntInf{negative=false, digits=digits}
      | abs i = i

    fun max arg = case compare arg of GREATER => #1 arg | _ => #2 arg
    fun min arg = case compare arg of LESS => #1 arg | _ => #2 arg

    fun sign (_IntInf{negative=true,...}) = ~1
      | sign (_IntInf{digits=[],...}) = 0
      | sign _ = 1

    fun sameSign (i,j) = sign i = sign j

    local
      fun fmt' fmtFn i =
            case i of
              (_IntInf{digits=[],...}) => "0"
            | (_IntInf{negative=true,digits}) => "~"^(fmtFn digits)
            | (_IntInf{negative=false,digits}) => fmtFn digits
    in
    fun fmt StringCvt.BIN = fmt' (BN.fmt2())
      | fmt StringCvt.OCT = fmt' (BN.fmt8())
      | fmt StringCvt.DEC = fmt' (BN.fmt10())
      | fmt StringCvt.HEX = fmt' (BN.fmt16())
    end

    fun toString a = fmt StringCvt.DEC a

    type ('a,'b) reader = ('a,'b) StringCvt.reader
    local
      fun scan' scanFn (getc:(char,'s)reader) (cs:'s) : (intinf * 's) option = let
            val cs' = NumScan.skipWS getc cs
            fun cvt (NONE,_) = NONE
              | cvt (SOME(i,cs),wr) = SOME(wr i, cs)
            in
              case (getc cs')
               of (SOME(#"~", cs'')) => cvt(scanFn getc cs'',zneg)
		| (SOME(#"-", cs'')) => cvt(scanFn getc cs'',zneg)
                | (SOME(#"+", cs'')) => cvt(scanFn getc cs'',posi)
                | (SOME _) => cvt(scanFn getc cs',posi)
                | NONE => NONE
              (* end case *)
            end
    in
    fun scan StringCvt.BIN = scan' (BN.scan2)
      | scan StringCvt.OCT = scan' (BN.scan8)
      | scan StringCvt.DEC = scan' (BN.scan10)
      | scan StringCvt.HEX = scan' (BN.scan16)
    end

    fun fromString s = StringCvt.scanString (scan StringCvt.DEC) s

    fun pow (_, 0) = one()
      | pow (i, ~1) = if abs(i) = one() then i else zero()
      | pow (_IntInf{negative=false,digits}, n) = posi(BN.exp(digits,n))
      | pow (_IntInf{negative=true,digits}, n) =
          if Int.mod (n, 2) = 0
            then posi(BN.exp(digits,n))
            else zneg(BN.exp(digits,n))

    fun log2 (_IntInf{negative=false,digits}) = Int31.toInt(BN.log2 digits)
      | log2 _ = raise Domain

(*
    fun hash (_IntInf{negative,digits}) : Int.int =
	let val sgn = if negative=false then 0w0 else 0w1
	    val sum = List.foldl (fn (i,w) => Word31.+ (w,Word31.fromInt i)) sgn digits
	in Word31.toIntX sum
	end
*)
    fun divMod (i,j) = divmod(i,j)
    fun quotRem (i,j) = quotrem(i,j)

(*    val base : Int.int = 0x40000000 *)

    structure I = Int31
    fun binary (f: I.int * I.int -> I.int, genSign:bool*bool->bool) (x:int, y:int) =
	let
	    val _IntInf{negative=sx,digits=xs} = x
	    val _IntInf{negative=sy,digits=ys} = y

	    val sign = genSign (sx, sy)

	    (* convert to two's complement;
	     * Compute (- x - borrow)
	     *)
	    fun twos (false, x:I.int, borrow:I.int) = (x, 0)
	      | twos (true, 0, 0) = (0, 0) (* no borrow *)
	      | twos (true, x, borrow) =
		(I.+(BN.nbase,I.+(x, borrow)), 0) (* borrow *)

	    (* convert to ones's complement *)
	    val ones = twos

	    fun loop ([], [], _, _, _) = []
	      | loop ([], y :: ys, bx, by, bz)  =
		loop1 (0, [], y, ys, bx, by, bz)
	      | loop (x :: xs, [], bx, by, bz) =
		loop1 (x, xs, 0, [], bx, by, bz)
	      | loop (x :: xs, y::ys, bx, by, bz) =
		loop1 (x, xs, y, ys, bx, by, bz)

	    and loop1 (x, xs, y, ys, bx, by, bz) =
		let (* convert from ones complement *)
		    val (x, bx) = twos (sx, x, bx)
		    val (y, by) = twos (sy, y, by)
		    val z  = f (x,y)
		    (* convert back to ones complement *)
		    val (z, bz) = ones (sign, z, bz)
		    val zs = loop (xs, ys, bx, by, bz)
		in
		    case (z, zs) of  (* strip leading zero *)
			(0, []) => []
		      | (z, zs) => z :: zs
		end
	in
	    case loop (xs, ys, 0, 0, 0) of
		[] => _IntInf{digits=[], negative=false}
	      | digits => _IntInf{negative=sign, digits=digits}
	end

    fun signOr (true,_) = true
      | signOr (_,true) = true
      | signOr _ = false

    fun signAnd (true,true) = true
      | signAnd _ = false

    fun signNeq (true,false) = true
      | signNeq (false,true) = true
      | signNeq _ = false

    val itow = Word31.fromInt o Int31.toInt
    val itow' = Word.fromInt o Int31.toInt
    val wtoi = Int31.fromInt o Word31.toIntX

    fun IntOp (opr:word31*word31->word31) (x:Int31.int,y:Int31.int) : Int31.int =
	wtoi(opr(itow x,itow y))

    fun orb x = binary (IntOp Word31.orb, signOr) x
    fun andb x = binary (IntOp Word31.andb, signAnd) x
    fun xorb x = binary (IntOp Word31.xorb, signNeq) x

    fun notb (i:int) = ~(i + fromInt 1)

    val baseBits = Int31.fromInt(Word31.toIntX(Word31.<< (0w15,0w1)))

    fun shiftAmount w =
	{ bytes = Int31.div (wtoi(Word31.fromLarge(Word.toLarge w)), baseBits),
	  bits = Int31.mod (wtoi(Word31.fromLarge(Word.toLarge w)), baseBits) }

    local
	infix || && << >>
	val op << = Word31.<<
	val op >> = Word31.>>
	val op && = Word31.andb
	val op || = Word31.orb
    in

	(* left shift; just shift the digits, no special treatment for
	 * signed versus unsigned. *)
	fun lshift (i, w) =
	    case i of
		_IntInf { digits = [], negative } => i (* i = 0 *)
	      | _IntInf { digits, negative } =>  let
		val { bytes, bits } = shiftAmount w
		val bits' = Int31.-(baseBits, bits)
		fun pad (0, xs) = xs
		  | pad (n, xs) = pad (Int31.-(n,1), 0 :: xs)
		fun shift ([], 0w0) : Int31.int list = []
		  | shift ([], carry) = [wtoi carry]
		  | shift (x :: xs, carry) = let
			val maxVal = itow (BN.maxDigit())
			val digit = ((itow x << itow' bits) || carry) && maxVal
			val carry' = itow x >> itow' bits'
		    in
			wtoi digit :: shift (xs, carry')
		    end
	    in
		    (_IntInf { negative=negative,
			  digits =
			  if bits = 0 then
			      pad (bytes, digits)
			  else
			      pad (bytes, shift (digits, 0w0)) })
	    end

	(* Right shift. *)
	fun rshift (i, w:word) =
	case i of
	    _IntInf { digits = [], negative } => i (* i = 0 *)
	  | _IntInf { digits, negative } => let
		val { bytes, bits } = shiftAmount w
		val bits' = Int31.-(baseBits, bits)
		fun drop (0, i) = i
		  | drop (n, []) = []
		  | drop (n, x :: xs) = drop (Int31.-(n,0), xs)
		fun shift [] = ([], 0w0)
		  | shift (x :: xs) =
		    let val (zs, borrow) = shift xs
			val z = borrow || (itow x >> itow' bits)
			val borrow' = (itow x << itow' bits') && (itow (BN.maxDigit()))
		    in
			(* strip leading 0 *)
			case (z, zs) of
			    (0w0, []) => ([], borrow')
			  | _ => (wtoi z :: zs, borrow')
		    end

		val digits =
		    if bits = 0 then drop (bytes, digits)
		    else #1 (shift (drop (bytes, digits)))
	    in
		case digits of
		    [] => _IntInf { negative=false, digits = [] }
		  | _ => _IntInf { negative=negative, digits = digits }
	    end
    end

    val << = lshift
    val ~>> = rshift
  end

(** SigDoc *)
structure LargeInt : INTEGER = IntInf
