(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07
 * GPL Licence
 *)

structure Pickle :> PICKLE = (* was : *)
  struct
    val sharing_p = true
    val linear_refs_p = true
    val very_safe_p = false
    val debug_p = false
    val comments_p = false

    structure S = Bitstream
    structure H = Polyhash
    structure Dyn = EqHashDyn

    fun id x = x

    fun log2 w =
        let fun f (w,n) =
                if w = 0w0 then n
                else f(Word.>>(w,0w1),n+0w1)
        in f(w,0w0)
        end

    exception PickleExn
    type dyn = Dyn.dyn

    datatype 'a cache = NoCache | Cached of 'a | Caching

    infix ==
    fun a == b : bool = false
(*
	let val a_i:int = Unsafe.cast a
	    val b_i:int = Unsafe.cast b
	in a_i = b_i
	end
*)

    fun fail s = let val s = "Pickle." ^ s
		 in print (s ^ "\n")
		  ; raise Fail s
		 end

    val maxDepth = 12
    val maxLength = 500

    local
	val Alpha = 0w65599
	val Beta = 0w19
    in
	fun hashAddSmall0 w acc = w+acc*Beta
	fun hashAdd w (acc,d) = (w+acc*Alpha,d-1)
	fun hashAddNoCount w (acc,d) = (w+acc*Alpha,d)
	fun hashAddSmall w (acc,d) = (w+acc*Beta,d-1)
	fun hashAddSmallNoCount w (acc,d) = (w+acc*Beta,d)
	fun hashComb f (p as (acc,d)) =
	    if d <= 0 then p
	    else f p
	val UNIT_HASH = 0w23
    end

    local val hashCount = ref 0w100
    in fun newHashCount() =
	!hashCount before hashCount := !hashCount + 0w1
    end

    fun stringHash acc s =
	let val sz = size s
	    val sz = if sz > maxLength then maxLength else sz
	    fun loop (n,a) =
		if n >= sz then a
		else loop (n+1,
			   hashAddSmall0
			   (Word.fromInt(Char.ord(String.sub(s,n)))) a)
	in loop (0,acc)
	end

    (* typ approximates the type of a pickler/unpickler. It is used for
     * prettyprinting (during debugging) and for avoiding (shareGen o
     * shareGen), (shareGen o dataGen), and (shareGen o refGen)... *)

    datatype typ = Tshare of typ | Tint | Tint31 | Tint32 | Treal | Tword | Tword1 | Tword8
                 | Tword32 | Tword31 | Tchar | Tcon0 | Tcon1 | Tunit | Tstring
                 | Tpair of typ * typ | Tref of typ | Tref0 of typ
                 | Tref1 of typ | Tconv of typ | Toption of typ | Tlist of typ | Tdecorate of string * typ
                 | TregisterEq of typ | Tdata of string * int | Tenum of int

    (* be conservative with typ_unboxed - don't look at Int.maxInt,
     * for instance, as this would course picklers compiled with
     * different sml-implementations to differ. *)

    fun typ_unboxed t =
        case t of Tint => true
                | Tint31 => true
                | Tword => true
                | Tword1 => true
                | Tword8 => true
                | Tword31 => true
                | Tchar => true
                | Tunit => true
                | _ => false

    fun pp_typ t =
        let fun arg s t acc = s :: "(" :: pp (t, ")"::acc)
            and pp (t, acc) =
                case t of
                  Tshare t => arg "Share" t acc
                | Tint => "i"::acc
                | Tint31 => "i31"::acc
                | Tint32 => "i32"::acc
                | Treal => "r"::acc
                | Tword => "w"::acc
                | Tword1 => "w1"::acc
                | Tword8 => "w8"::acc
                | Tword31 => "w31"::acc
                | Tword32 => "w32"::acc
                | Tchar => "c"::acc
                | Tcon0 => "C0"::acc
                | Tcon1 => "C1"::acc
                | Tunit => "1"::acc
                | Tstring => "s"::acc
                | Tpair(t1,t2) => "("::pp(t1,","::pp(t2,")"::acc))
                | Tref t => arg "ref" t acc
                | Tref0 t => arg "ref0" t acc
                | Tref1 t => arg "ref1" t acc
                | Tconv t => arg "conv" t acc
                | Toption t => arg "option" t acc
                | Tlist t => arg "list" t acc
                | Tdecorate(s,t) => arg ("deco_" ^ s) t acc
                | TregisterEq t => arg "registerEq" t acc
                | Tdata (s,i) => "data(" :: s :: "," :: Int.toString i :: ")" :: acc
                | Tenum i => "enum(" :: Int.toString i :: ")" :: acc

        in String.concat(pp(t,nil))
        end

    structure TableFactory :> sig
      type 'a t_from
      type 'a t_to
      val mkFrom     : {hash: 'a -> word, eq: 'a * 'a -> bool} -> 'a t_from
      val mkTo       : unit -> 'a t_to
      val addTo      : 'a t_to -> word -> 'a -> unit
      val addFrom    : 'a t_from -> 'a -> word -> unit
      val addFromCheck : 'a t_from -> 'a -> word -> unit
      val lookupTo   : 'a t_to -> word -> 'a option
      val lookupFrom : 'a t_from -> 'a -> word option
      val reset      : unit -> unit
      val clearTo    : 'a t_to -> unit
      val clearFrom  : 'a t_from -> unit
      val register_reset : (unit -> unit) -> unit
      val bitsFrom   : 'a t_from -> word
      val bitsTo     : 'a t_to -> word
      val checkFrom  : {typ:typ,pp:'a->string} -> 'a t_from -> 'a -> unit
    end = struct
      type 'a t_from = ('a, word) H.hash_table
      type 'a t_to = (word, 'a) H.hash_table
      val reset_fun = ref id
      fun register_reset f = reset_fun := (f o !reset_fun)
      fun mkFrom {hash: 'a -> word, eq: 'a * 'a -> bool} : 'a t_from =
          H.mkTable (Word.toIntX o hash, eq) (10,PickleExn)
      fun mkTo() =
          H.mkTable (Word.toIntX, op =) (10,PickleExn)
      fun addTo t k v = H.insert t (k,v)
      fun addFrom t k v = H.insert t (k,v)
      fun lookupTo t = H.peek t
      fun lookupFrom t = H.peek t
      fun clearTo t = H.clear t
      fun clearFrom t = H.clear t
      fun reset() = !reset_fun ()
      fun bitsFrom t = (log2 o Word.fromInt o H.numItems) t
      val bitsTo = bitsFrom
      fun checkFrom {typ:typ, pp:'a->string} (t:'a t_from) (v:'a) : unit =
          let val (c,h) = H.peekSameHash t v
	      val maxBucket = 100
	  in if c > maxBucket then
               print ("** Bucket > " ^ Int.toString maxBucket
		      ^ " (c=" ^ Int.toString c ^ ",h="
		      ^ Int.toString h ^") **: " ^ pp_typ typ ^ "\n"
		      ^ "** Value = " ^ pp v ^ "\n")
	     else ()
          end
      fun addFromCheck t k v =
          let val (c,_) = H.peekSameHash t k
	      val maxBucket = 10
	  in if c > maxBucket then ()
             else H.insert t (k,v)
          end
    end

    structure HashConsEnv :>
	sig
	    type hce
	    val empty : unit -> hce
	    val add : hce -> dyn -> dyn
	end =
    struct
	type hce = (dyn, dyn) H.hash_table
	fun empty() =
	    H.mkTable (Word.toIntX o Dyn.hash maxDepth, Dyn.eq) (10,PickleExn)
	fun add hce d =
	    case H.peek hce d of
		SOME d => d
	      | NONE => (H.insert hce (d,d); d)
    end


    type hce = HashConsEnv.hce
    val empty_hce = HashConsEnv.empty

    type instream = S.instream * hce
    type outstream = S.outstream
    type 'a pickler = 'a -> outstream -> outstream
    type 'a unpickler = instream -> 'a * instream
    type 'a hasher = 'a -> word*int -> word*int    (* int: hash depth *)
    type 'a eq = 'a * 'a -> bool


    (* Datatype to force region inference in the ML Kit to infer fewer
     * region parameters for the polymorphic combinators... *)

    datatype 'a pu = PU of {pickler   : 'a pickler,
			    unpickler : 'a unpickler,
			    hasher    : 'a hasher,
			    eq        : 'a eq,
			    typ       : typ}

    fun pickler (PU pu:'a pu) = #pickler pu
    fun unpickler (PU pu:'a pu) = #unpickler pu
    fun hasher (PU pu:'a pu) = #hasher pu
    fun typ (PU pu:'a pu) = #typ pu
    fun eQ (PU pu:'a pu) = #eq pu
    fun hashfun (pu:'a pu) : 'a -> word =
        fn v => (#1(hasher pu v (0w0,maxDepth)))

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
	    in PU
	       {pickler = fn v => fn s =>
			   let val s = S.outw32 (c,s)
			   in pickler pu v s
			   end,
		unpickler = fn (s,hce) =>
			     let val (w,s) = S.getw32 s
			     in if w <> c then fail ("debug.expected " ^ str)
				else unpickler pu (s,hce)
			     end,
		hasher = hasher pu,
		eq = eQ pu,
		typ = typ pu}
	    end
    end

    fun word32Gen s (toWord:''a->Word32.word, fromWord:Word32.word->''a) : ''a pu =
	debug s (PU
	{pickler = fn w => fn s => S.outcw32 (toWord w,s),
	 unpickler = fn (s,hce) => let val (w,s) = S.getcw32 s
			           in (fromWord w, (s,hce))
			           end,
	 hasher = fn a => hashComb (fn p => hashAdd (w32_to_w(toWord a)) p),
	 eq = op =,
	 typ = Tword32})

    fun wordGen s (toWord:''a->Word.word, fromWord:Word.word->''a) : ''a pu =
	debug s (PU
	{pickler = fn w => fn s => S.outcw (toWord w,s),
	 unpickler = fn (s,hce) => let val (w,s) = S.getcw s
			           in (fromWord w, (s,hce))
			           end,
	 hasher = fn a => hashComb (fn p => hashAdd (toWord a) p),
	 eq = op =,
	 typ = Tword})

    fun charGen s (toChar:''a->char, fromChar:char->''a) : ''a pu =
	debug s (PU
	{pickler = fn v => fn s => S.outc (toChar v,s),
	 unpickler = fn (s,hce) => let val (c,s) = S.getc s
			           in (fromChar c, (s,hce))
			           end,
	 hasher = fn a => hashComb (fn p => hashAdd (Word.fromInt(Char.ord(toChar a))) p),
	 eq = op =,
	 typ = Tchar})

    fun w1Gen s (tow1:''a->Word8.word, fromw1:Word8.word->''a) : ''a pu =
	debug s (PU
	{pickler = fn v => fn s => S.outw1 (tow1 v,s),
	 unpickler = fn (s,hce) => let val (c,s) = S.getw1 s
				       in (fromw1 c, (s,hce))
				       end,
	 hasher = fn a => hashComb (fn p => hashAdd (Word.fromInt(Word8.toInt(tow1 a))) p),
	 eq = op =,
	 typ = Tword1})

    val word = wordGen "word" (id,id)
    val word32 = word32Gen "word32" (id,id)

    (* allow for serialization with Int=Int31 to be deserialized with Int=Int32! *)

    val int = word32Gen "int" (Word32.fromInt, Int.fromLarge o Word32.toLargeIntX)
    val int32 = word32Gen "int32" (Word32.fromLargeInt o Int32.toLarge,
				   Int32.fromLarge o Word32.toLargeIntX)

    val char = charGen "char" (id,id)

    val bool = w1Gen "bool" (fn true => 0w1 | false => 0w0,
			     fn 0w0 => false | _ => true)

    val REF : Word8.word = 0w0
    val DEF : Word8.word = 0w1

    fun mkTables hash eq =
        let val clearRef = ref false
            val _ = TableFactory.register_reset (fn () => (clearRef := true))
            val count = ref 0w0
            fun reset () = count := 0w0
            val _ = TableFactory.register_reset reset
            fun newloc () = !count before (count := !count + 0w1)
            val T_to = TableFactory.mkTo()
            val T_from = TableFactory.mkFrom {hash=hash, eq=eq}
            fun maybeClear() =
                if !clearRef then (TableFactory.clearTo T_to;
                                   TableFactory.clearFrom T_from;
                                   clearRef := false)
                else ()
        in {newloc=newloc,T_to=T_to,T_from=T_from,maybeClear=maybeClear}
        end

    fun shareGen0 (pp: 'a -> string) (pu:'a pu) : 'a pu =
      if not sharing_p then pu else
      debug "shareGen0"
      let val {newloc,T_to,T_from,maybeClear} = mkTables (hashfun pu) (eQ pu)
          val typ = Tshare(typ pu)
      in PU
          {pickler = fn v => fn s =>
		      let val _ = maybeClear()
		      in case TableFactory.lookupFrom T_from v of
			  SOME loc =>
                              let val s = S.outw1(REF,s)
                                  val s = S.outcw(loc,s)
(*
                                  val bits = TableFactory.bitsFrom T_from
                                  val s = S.outwN' bits (loc,s)
*)
			      in s
			      end
			| NONE =>
			      let val s = S.outw1(DEF,s)
                                  val loc = newloc()
				  (* do insert after the pickling    *)
	  		          (*  - otherwise there are problems *)
			          (*    with cycles.                 *)
				  val res = pickler pu v s
			      in case TableFactory.lookupFrom T_from v of
				  SOME _ => res
				| NONE =>
				      let (*val () = TableFactory.checkFrom {pp=pp,typ=typ} T_from v*)
				      in TableFactory.addFromCheck T_from v loc
				       ; res
				      end
			      end
		      end,
	   unpickler = fn (s,hce) =>
			let val _ = maybeClear()
                            val (tag,s) = S.getw1 s
			in if tag = REF then
			    let val (loc,s) = S.getcw s
(*
                                val bits = TableFactory.bitsTo T_to
                                val (loc,s) = S.getwN' bits s
*)
			    in case TableFactory.lookupTo T_to loc of
				SOME v => (v, (s,hce))
			      | NONE => fail ("shareGen0.impossible, loc="
					      ^ Word.toString loc (*^
                                              "bits=" ^ Word.toString bits*))
			    end
			   else if tag = DEF then
			       let val loc = newloc()
				   val (v,(s,hce)) = unpickler pu (s,hce)

				   val _ = case TableFactory.lookupTo T_to loc of
				       NONE => TableFactory.addTo T_to loc v
				     | SOME _ => () (*fail ("shareGen0New.Location "
						       ^ Word.toString loc
						       ^ " already there!") *)
			       in (v, (s,hce))
			       end
			   else fail "shareGen0.impossible2"
			end,
	   hasher = hasher pu,
	   eq = eQ pu,
	   typ = typ}
      end

    fun shareGen a =
        let val t_a = typ a
            fun warn() = print ("Pickle warning: shareGen applied to " ^ pp_typ t_a ^ "\n")
                         (* MEMO: Here, we could also just dynamically - at pickler
                          * construction time - strip one of the share combinators *)
            val _ = case t_a of Tshare _ => warn()
                              | Tref _ => warn()
                              | Tref0 _ => warn()
                              | Tref1 _ => warn()
                              | Tdata _ => warn()
                              | _ => if typ_unboxed t_a then warn() else ()
        in
          shareGen0 (fn _ => "no pp") a
        end

    val string : string pu =
	(shareGen o debug "string" o PU)
	{pickler = (fn st => fn s =>
		    let val sz = size st
			val s = S.outcw(Word.fromInt sz, s)
			val s = CharVector.foldl (fn (c,cs) => S.outc(c,cs)) s st
		    in s
		    end),
	 unpickler = (fn (s,hce) =>
		      let val (sz,s) = S.getcw s
			  val sz = Word.toInt sz
			  fun read (0,s,acc) = (implode(rev acc), s)
			    | read (n,s,acc) =
			      let val (c,s) = S.getc s
			      in read (n-1, s, c :: acc)
			      end
			  val (st,s) = read (sz,s,nil)
		      in  (st, (s,hce))
		      end),
	 hasher = fn s => hashComb (fn (acc, d) => (stringHash acc s, d-1)),
	 eq = op =,
	 typ = Tstring}

    fun pairGen0 (pu1 :'a pu, pu2 :'b pu) : ('a * 'b) pu =
	let val hash_pair = newHashCount()
	in
	    debug "pair" (PU
	    {pickler = (fn (v1:'a,v2:'b) => fn os =>
			let val os = pickler pu1 v1 os
			in pickler pu2 v2 os
			end),
	     unpickler = (fn is =>
			  let val (v1,is) = unpickler pu1 is
			      val (v2,is) = unpickler pu2 is
			  in ((v1,v2), is)
			  end),
	     hasher = (fn (a,b) =>
		       hashComb (fn p =>
				 let val p = hashAddSmallNoCount
				       hash_pair (hasher pu1 a p)
				 in hashComb (hasher pu2 b) p
				 end)),
	     eq = fn (p1 as (a1,a2),p2 as (b1,b2)) =>
	             p1==p2
	             orelse eQ pu1 (a1,b1) andalso eQ pu2 (a2,b2),
	     typ = Tpair(typ pu1,typ pu2)})
	end

    fun pairGen pu = shareGen(pairGen0 pu)

    fun refEqGen (eq: 'a ref * 'a ref -> bool)
	(v_dummy:'a) (pu:'a pu) : 'a ref pu =
      debug "refEqGen"
      let
          val {newloc,T_to,T_from,maybeClear} = mkTables (hashfun pu o !) eq
	  val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
	  val typ = Tref(typ pu)
      in PU
          {pickler = fn r as ref v => fn s =>
		      let val _ = maybeClear()
		      in case TableFactory.lookupFrom T_from r of
			  SOME loc =>
			      let val s = S.outw1(REF,s)
                                  val s = S.outcw2(loc,s)
			      in s
			      end
			| NONE =>
			      let val s = S.outw1(DEF,s)
                                  val loc = newloc()
                                  (*val () = TableFactory.checkFrom {pp=fn _ => "no pp (ref)",typ=typ} T_from r*)
			      in TableFactory.addFrom T_from r loc
			       ; pickler pu v s
			      end
		      end,
	   unpickler = fn (s,hce) =>
			   let val _ = maybeClear()
                               val (tag,s) = S.getw1 s
		       	   in if tag = REF then
			    let val (loc,s) = S.getcw2 s
			    in case TableFactory.lookupTo T_to loc of
				SOME r => (r, (s,hce))
			      | NONE => fail "ref.impossible"
			    end
			   else (* tag = DEF *)
			       let val loc = newloc()
				   val r = ref v_dummy
				   val _ = TableFactory.addTo T_to loc r
				   val (v,(s,hce)) = unpickler pu (s,hce)
			       in r := v ; (r, (s,hce))
			       end
			   end,
	   hasher = href,
	   eq = eq,
	   typ = typ}
      end

    fun refGen (v_dummy:'a) (pu:'a pu) : 'a ref pu =
	refEqGen (op =) v_dummy pu

    fun ref0EqGenPP (pp : 'a ref -> string) (eq: 'a ref * 'a ref ->bool) (pu:'a pu) : 'a ref pu =
      debug "ref0EqGen"
      let val eq = if very_safe_p then op = else eq
	  val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
	  val typ = Tref0(typ pu)
          val {newloc,T_to,T_from,maybeClear} = mkTables (hashfun pu o !) eq
      in PU
          {pickler = fn r as ref v => fn s =>
		      let val _ = maybeClear()
		      in case TableFactory.lookupFrom T_from r of
			  SOME loc =>
			      let val s = S.outw1(REF,s)
                                  val s = S.outcw2(loc,s)
			      in s
			      end
			| NONE =>
			      let val s = S.outw1(DEF,s)
                                  val loc = newloc()
                                  (*val () = TableFactory.checkFrom {pp=pp,typ=typ} T_from r*)
			      in TableFactory.addFrom T_from r loc
			       ; pickler pu v s
			      end
		      end,
	   unpickler = fn (s,hce) =>
			let val _ = maybeClear()
                            val (tag,s) = S.getw1 s
			in if tag = REF then
			    let val (loc,s) = S.getcw2 s
			    in case TableFactory.lookupTo T_to loc of
				SOME r => (r, (s,hce))
			      | NONE => fail "ref.impossible"
			    end
			   else (* tag = DEF *)
			       let val loc = newloc()
				   val (v,(s,hce)) = unpickler pu (s,hce)
				   val r = ref v
			       in TableFactory.addTo T_to loc r
				; (r, (s,hce))
			       end
			end,
	   hasher = href,
	   eq = eq,
	   typ = typ}
      end

    fun ref0EqGen eq pu =
        ref0EqGenPP (fn _ => "no pp (ref0EqGen)") eq pu

    fun ref0Gen (pu:'a pu) : 'a ref pu =
	ref0EqGen (op =) pu

    fun ref0GenPP (pp:'a ref->string) (pu:'a pu) : 'a ref pu =
	ref0EqGenPP pp (op =) pu

    fun ref0ShGen (pu:'a pu) : 'a ref pu =
	if very_safe_p then ref0Gen pu
	else ref0EqGen (fn (ref a,ref b) => eQ pu (a,b)) pu

    fun refOneGen (pu:'a pu) : 'a ref pu =    (* Only works when sharing is enabled! *)
      if not sharing_p orelse not linear_refs_p then ref0Gen pu
      else
      let val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
      in
	  debug "refOneGen" (PU
	  {pickler = fn r as ref v => pickler pu v,
	   unpickler = (fn is => let val (v,is) = unpickler pu is
				 in (ref v, is)
				 end),
	   hasher = href,
	   eq = op =,
	   typ = Tref1(typ pu)})
      end

    fun necessary_bits_for_constructors sz : word =
        let val w = Word.fromInt sz
        in log2(w-0w1)
        end
(*
                        case sz of
                          1 => 0w0
                        | 2 => 0w1
                        | 3 => 0w2
                        | 4 => 0w2
                        | 5 => 0w3
                        | 6 => 0w3
                        | 7 => 0w3
                        | 8 => 0w3
*)

    fun dataGen0 typ (name, toInt: 'a -> int, fs : ('a pu -> 'a pu) list) : 'a pu =
	debug "dataGen"
	let (* val _ = print ("Generated pickler for " ^ name ^ "\n") *)
            val typ = case typ of SOME typ => typ
                                | NONE => Tdata(name,length fs)
	    val hash_data = newHashCount()
	    val res : 'a pu option ref = ref NONE
	    val ps : 'a pu Vector.vector cache ref = ref NoCache
            val fs_sz = length fs
	    fun p() =
                if fs_sz = 1 then fn x => pickler (getPUPI 0) x
                else if fs_sz <= 256 then
                  let val bits = necessary_bits_for_constructors fs_sz
                  in fn v => fn s =>
                       let val i = toInt v
                           val s = S.outwN bits (Word8.fromInt i,s)
                       in pickler (getPUPI i) v s
	               end
                  end
                else
                  (fn v => fn s =>
	             let val i = toInt v
		         val s = S.outcw2 (Word.fromInt i, s)
	             in pickler (getPUPI i) v s
	             end)
            and up() =
                if fs_sz = 1 then fn x => unpickler (getPUPI 0) x
                else if fs_sz <= 256 then
                  let val bits = necessary_bits_for_constructors fs_sz
                  in fn (s,hce) =>
	                let val (w,s) = S.getwN bits s
	                in unpickler (getPUPI (Word8.toInt w)) (s,hce)
	                end
                  end
                else
                  (fn (s,hce) =>
	             let val (w,s) = S.getcw2 s
	             in unpickler (getPUPI (Word.toInt w)) (s,hce)
	             end)
	    and eq(a1:'a,a2:'a) : bool =
		a1 == a2 orelse
		let val n = toInt a1
		in n = toInt a2 andalso eQ (getPUPI n) (a1,a2)
		end
	    and getPUP() =
		case !res of
		    NONE => let (* val typ = name ^ "_" ^ Int.toString (length fs) *)
				fun pp v = "Con" ^ Int.toString (toInt v)
				val pup = shareGen0 pp (PU {pickler=p(),unpickler=up(),
							    hasher=h,eq=eq,typ=typ})
			    in res := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and getPUPI (i:int) =
		case !ps of
		    NoCache => let val _ = ps := Caching
				   val ps0 = map (fn f => f (getPUP())) fs
				   val psv = Vector.fromList ps0
			       in ps := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen.Caching: " ^ name)
	    and h v =
		hashComb (fn p =>
			  let val i = toInt v
			      val h_arg = hasher (getPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount hash_data (h_arg v p))
			  end)
        in getPUP()
	end

    fun dataGen a = dataGen0 NONE a

    fun data2Gen (aname, aToInt: 'a -> int, afs : ('a pu * 'b pu -> 'a pu) list,
		  bname, bToInt: 'b -> int, bfs : ('a pu * 'b pu -> 'b pu) list)
	: 'a pu * 'b pu =
	let (* val _ = print ("Generated pickler for " ^ aname ^ "/" ^ bname ^ "\n") *)
	    val aHashData = newHashCount()
	    val bHashData = newHashCount()
	    val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector cache ref = ref NoCache
	    val bPs : 'b pu Vector.vector cache ref = ref NoCache
	    fun aP v s =
	      let val i = aToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in pickler(aGetPUPI i) v s
	      end
            and aUp (s,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(aGetPUPI (Word.toInt w)) (s,hce)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		a1==a2 orelse
		let val n = aToInt a1
		in n = aToInt a2 andalso eQ (aGetPUPI n) (a1,a2)
		end
	    and aGetPUP() =
		case !aRes of
		    NONE => let (*val typ = aname ^ "_" ^ Int.toString (length afs)*)
                                val typ = Tdata(aname,length afs)
				fun pp v = "Con" ^ Int.toString (aToInt v)
				val pup = shareGen0 pp (PU {pickler=aP,unpickler=aUp,
							    hasher=aH,eq=aEq,typ=typ})
			    in aRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and aGetPUPI (i:int) =
		case !aPs of
		    NoCache => let val _ = aPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP())) afs
				   val psv = Vector.fromList ps0
			       in aPs := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen2.Caching.a: " ^ aname)
	    and bP v s =
	      let val i = bToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in pickler(bGetPUPI i) v s
	      end
            and bUp (s,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(bGetPUPI (Word.toInt w)) (s,hce)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		b1==b2 orelse
		let val n = bToInt b1
		in n = bToInt b2 andalso eQ (bGetPUPI n) (b1,b2)
		end
	    and bGetPUP() =
		case !bRes of
		    NONE => let (*val typ = bname ^ "_" ^ Int.toString (length bfs)*)
                                val typ = Tdata(bname,length bfs)
				fun pp v = "Con" ^ Int.toString (bToInt v)
				val pup = shareGen0 pp (PU {pickler=bP,unpickler=bUp,
							    hasher=bH,eq=bEq,typ=typ})
			    in bRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and bGetPUPI (i:int) =
		case !bPs of
		    NoCache => let val _ = bPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP())) bfs
				   val psv = Vector.fromList ps0
			       in bPs := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen2.Caching.b: " ^ bname)
	    and aH v =
		hashComb (fn p =>
			  let val i = aToInt v
			      val h_arg = hasher (aGetPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount aHashData (h_arg v p))
			  end)
	    and bH v =
		hashComb (fn p =>
			  let val i = bToInt v
			      val h_arg = hasher (bGetPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount bHashData (h_arg v p))
			  end)
        in (debug "data2Gen.a" (aGetPUP()), debug "data2Gen.b" (bGetPUP()))
	end

    fun data3Gen (aname, aToInt: 'a->int, afs : ('a pu*'b pu*'c pu->'a pu)list,
		  bname, bToInt: 'b->int, bfs : ('a pu*'b pu*'c pu->'b pu)list,
		  cname, cToInt: 'c->int, cfs : ('a pu*'b pu*'c pu->'c pu)list)
	: 'a pu * 'b pu * 'c pu =
	let (* val _ = print ("Generated pickler for " ^ aname ^ "/" ^ bname ^ "/" ^ cname ^ "\n") *)
	    val aHashData = newHashCount()
	    val bHashData = newHashCount()
	    val cHashData = newHashCount()
	    val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val cRes : 'c pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector cache ref = ref NoCache
	    val bPs : 'b pu Vector.vector cache ref = ref NoCache
	    val cPs : 'c pu Vector.vector cache ref = ref NoCache
	    fun aP v s =
	      let val i = aToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in pickler(aGetPUPI i) v s
	      end
            and aUp (s,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(aGetPUPI (Word.toInt w)) (s,hce)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		a1==a2 orelse
		let val n = aToInt a1
		in n = aToInt a2 andalso eQ (aGetPUPI n) (a1,a2)
		end
	    and aGetPUP() =
		case !aRes of
		    NONE => let (*val typ = aname ^ "_" ^ Int.toString (length afs)*)
                                val typ = Tdata(aname,length afs)
				fun pp v = "Con" ^ Int.toString (aToInt v)
				val pup = shareGen0 pp (PU {pickler=aP,unpickler=aUp,
							    hasher=aH,eq=aEq,typ=typ})
			    in aRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and aGetPUPI (i:int) =
		case !aPs of
		    NoCache => let val _ = aPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),
							     cGetPUP())) afs
				   val psv = Vector.fromList ps0
			       in aPs := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen3.Caching.a: " ^ aname)
	    and bP v s =
	      let val i = bToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in pickler(bGetPUPI i) v s
	      end
            and bUp (s,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(bGetPUPI (Word.toInt w)) (s,hce)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		b1==b2 orelse
		let val n = bToInt b1
		in n = bToInt b2 andalso eQ (bGetPUPI n) (b1,b2)
		end
	    and bGetPUP() =
		case !bRes of
		    NONE => let (*val typ = bname ^ "_" ^ Int.toString (length bfs)*)
                                val typ = Tdata(bname,length bfs)
				fun pp v = "Con" ^ Int.toString (bToInt v)
				val pup = shareGen0 pp (PU {pickler=bP,unpickler=bUp,
							    hasher=bH,eq=bEq,typ=typ})
			    in bRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and bGetPUPI (i:int) =
		case !bPs of
		    NoCache => let val _ = bPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),
							     cGetPUP())) bfs
				   val psv = Vector.fromList ps0
			       in bPs := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen3.Caching.b: " ^ bname)
	    and cP v s =
	      let val i = cToInt v
		  val s = S.outcw (Word.fromInt i, s)
	      in pickler(cGetPUPI i) v s
	      end
            and cUp (s,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(cGetPUPI (Word.toInt w)) (s,hce)
	      end
	    and cEq(c1:'c,c2:'c) : bool =
		c1==c2 orelse
		let val n = cToInt c1
		in n = cToInt c2 andalso eQ (cGetPUPI n) (c1,c2)
		end
	    and cGetPUP() =
		case !cRes of
		    NONE => let (*val typ = cname ^ "_" ^ Int.toString (length cfs)*)
                                val typ = Tdata(cname,length cfs)
				fun pp v = "Con" ^ Int.toString (cToInt v)
				val pup = shareGen0 pp (PU {pickler=cP,unpickler=cUp,
							    hasher=cH,eq=cEq,typ=typ})
			    in cRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and cGetPUPI (i:int) =
		case !cPs of
		    NoCache => let val _ = cPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),
							     cGetPUP())) cfs
				   val psv = Vector.fromList ps0
			       in cPs := Cached psv
				   ; Vector.sub(psv,i)
			       end
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen3.Caching.c: " ^ cname)
	    and aH v =
		hashComb (fn p =>
			  let val i = aToInt v
			      val h_arg = hasher (aGetPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount aHashData (h_arg v p))
			  end)
	    and bH v =
		hashComb (fn p =>
			  let val i = bToInt v
			      val h_arg = hasher (bGetPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount bHashData (h_arg v p))
			  end)
	    and cH v =
		hashComb (fn p =>
			  let val i = cToInt v
			      val h_arg = hasher (cGetPUPI i)
			  in hashAddSmallNoCount (Word.fromInt i)
			      (hashAddSmallNoCount cHashData (h_arg v p))
			  end)
        in (debug "data3Gen.a" (aGetPUP()),
	    debug "data3Gen.b" (bGetPUP()),
	    debug "data3Gen.c" (cGetPUP()))
	end

    fun con0 (b: 'b) (_: 'a) =
	PU {pickler = fn _ => fn s => s,
	    unpickler = fn is => (b,is),
	    hasher = fn _ => fn p => p,
	    eq = fn _ => true,       (* tag is checked with toInt in dataNGen *)
	    typ = Tcon0}

    fun con1 (con:'a->'b) (decon: 'b->'a) (pu: 'a pu) =
	PU {pickler = fn b:'b => pickler pu (decon b),
	    unpickler = (fn is =>
			 let val (a,is) = unpickler pu is
			 in (con a,is)
			 end),
	    hasher = fn b:'b => hashComb (fn p => hasher pu (decon b) p),
	    eq = fn (b1:'b, b2:'b) => eQ pu (decon b1, decon b2),
	    typ = Tcon1}

    fun newHash (f: 'a -> int) (pu: 'a pu) : 'a pu =
	PU {pickler= pickler pu,
	    unpickler = unpickler pu,
	    hasher = hashComb o hashAdd o Word.fromInt o f,
	    eq= eQ pu,
	    typ = typ pu}

    fun maybeNewHash (f: 'a -> int option) (pu: 'a pu) : 'a pu =
	PU {pickler= pickler pu,
	    unpickler = unpickler pu,
	    hasher = fn a => hashComb (fn p =>
				       case f a of
					   SOME i => hashAdd (Word.fromInt i) p
					 | NONE => hasher pu a p),
	    eq= eQ pu,
	    typ = typ pu}

    fun combHash (f: 'a -> int) (pu: 'a pu) : 'a pu =
	PU {pickler= pickler pu,
	    unpickler = unpickler pu,
	    hasher = fn a:'a => hashComb (fn p =>
					  let val p = hashAdd (Word.fromInt(f a)) p
					  in hashComb (hasher pu a) p
					  end),
	    eq= eQ pu,
	    typ = typ pu}

    fun listGen (pu_a: 'a pu) : 'a list pu =
	(debug "list" o combHash length)
	let fun toInt nil = 0
	      | toInt (op :: _) = 1
	    val f_nil = con0 nil
	    fun f_cons pu =
		con1 (op ::) (fn op :: p => p | _ => fail "cons")
		(pairGen0(pu_a,pu))
	in dataGen0 (SOME(Tlist(typ pu_a))) ("list",toInt,[f_nil,f_cons])
	end

    fun optionGen (pu_a: 'a pu) : 'a option pu =
	debug "option"
	let fun toInt NONE = 0
	      | toInt (SOME _) = 1
	    val fun_NONE = con0 NONE
	    fun fun_SOME _ =
		con1 SOME (fn SOME v => v | NONE => fail "option") pu_a
	in dataGen0 (SOME(Toption(typ pu_a))) ("option",toInt,[fun_NONE,fun_SOME])
	end

    fun enumGen (name, xs: ''a list) : ''a pu =   (*inefficient*)
	debug "enum"
	let val (wxs,n) =
	    List.foldl (fn (x, (wxs, n)) =>
			((x,Word.fromInt n)::wxs, n+1))
	    (nil,0) xs
	    fun lookupw nil _ = fail "enumGen.unknown constructor"
	      | lookupw ((x,w)::xs) v = if x=v then w else lookupw xs v
	    fun lookupv nil _ = fail "enumGen.unknown constructor tag"
	      | lookupv ((x,w)::xs) w0 = if w=w0 then x else lookupv xs w0
	    val hash_enum = newHashCount()
	in PU
	    {pickler = fn v => fn s => S.outcw(lookupw wxs v,s),
	     unpickler = (fn (s,hce) =>
			  let val (w,s) = S.getcw s
			  in (lookupv wxs w, (s,hce))
			  end),
	     hasher = (fn v => hashComb (fn p => hashAddSmallNoCount hash_enum
					 (hashAddSmallNoCount (lookupw wxs v) p))),
	     eq = op =,
	     typ = Tenum(length xs)}
	end

    fun pickle pu v =
        let val _ = TableFactory.reset();
            val os = S.openOut()
            val os = pickler pu v os
        in S.toString os
        end

    fun unpickle pu s =
        let val _ = TableFactory.reset()
            val (v, _) = unpickler pu (S.openIn s, empty_hce())
        in v
        end

    fun unpickle' pu hce s =
        let val _ = TableFactory.reset()
            val (v, (_,hce)) = unpickler pu (S.openIn s,hce)
        in (v, hce)
        end

    fun convert0 (to: 'a->'b ,back: 'b->'a) (pu:'a pu) : 'b pu =
	let val hash_conv = newHashCount()
	in PU
	    {pickler = fn v => fn s => pickler pu (back v) s,
	     unpickler = (fn is => let val (v,is) = unpickler pu is
				   in (to v,is)
				   end),
	     hasher = fn v => hashComb (fn p => hashAddSmallNoCount
					hash_conv ((hasher pu o back) v p)),
	     eq = fn (x,y) => eQ pu (back x, back y),
	     typ = Tconv(typ pu)}
	end

    fun convert a b = shareGen(convert0 a b)

    fun tup3Gen0 (a,b,c) =
	debug "tup3"
	let fun to (a,(b,c)) = (a,b,c)
	    fun from (a,b,c) = (a,(b,c))
	in convert0 (to,from) (pairGen0(a,pairGen0(b,c)))
	end

    fun tup4Gen0 (a,b,c,d) =
	debug "tup4"
	let fun to ((a,b),(c,d)) = (a,b,c,d)
	    fun from (a,b,c,d) = ((a,b),(c,d))
	in convert0 (to,from) (pairGen0(pairGen0(a,b),pairGen0(c,d)))
	end

    fun tup3Gen a = shareGen(tup3Gen0 a)
    fun tup4Gen a = shareGen(tup4Gen0 a)

    fun vectorGen pu =
	convert (Vector.fromList,Vector.foldr (op ::) nil)
	(listGen pu)

(*
    val real =
	debug "real"
	(convert (fn s => PackRealBig.fromBytes(Byte.stringToBytes s),
		  fn r => Byte.bytesToString(PackRealBig.toBytes r))
	 string)
*)

    val real =
	debug "real"
	(convert (fn s => case Real.fromString s of
                            SOME r => r
                          | NONE => fail "real.impossible",
		  Real.toString)
	 string)

    val time =
	debug "time" (convert (Time.fromReal,Time.toReal) real)

    val unit : unit pu =
	PU {pickler = fn () => id,
	    unpickler = fn is => ((),is),
	    hasher = fn () => hashComb (fn p => hashAddSmallNoCount UNIT_HASH p),
	    eq = fn _ => true,
	    typ = Tunit}

    fun cache0 (s:string) : ('a -> 'b) -> 'a -> 'b =
	let (* val _ = print ("caching: " ^ s ^ "\n") *)
	    val C : 'b cache ref = ref NoCache
	in  fn f : 'a -> 'b =>
	    fn a : 'a =>
	    case !C of
		Cached v => v
	      | NoCache =>
		    (C := Caching;
		     let val v : 'b = f a
		     in case !C of
			 Caching =>
			     (  C := Cached v
			      ; v)
		       | _ => fail "cacheNew impossible"
		     end)
	     | Caching =>
		    fail ("recursive cache appearance for " ^ s)
	end

    fun cache (s:string) : ('a -> 'b pu) -> 'a -> 'b pu = cache0 s
    fun cache2 (s:string) : ('a -> 'b pu * 'c pu) -> 'a -> 'b pu * 'c pu = cache0 s

    fun registerEq (eq: 'a * 'a -> bool) (key : 'a -> int)
	(debug_str:string) (vs: 'a list) (pu : 'a pu) : 'a pu =
	let val h : ('a,word) H.hash_table = H.mkTable (key, eq) (10,PickleExn)
(*	    val _ = print ("registerEq: " ^ debug_str ^ "\n") *)
	    val _ = List.foldl (fn (e,n) => (H.insert h (e,n); n + 0w1)) 0w1 vs
	    val v = Vector.fromList vs
	    fun lookup w =
		let val i = Word.toInt w - 1
		in Vector.sub(v,i)
		end
	    val NOT_THERE : word = 0w0
	in PU
	   {pickler = (fn v => fn s =>
		       case H.peek h v of
			   SOME w => S.outcw(w,s)
			 | NONE => let val s = S.outcw(NOT_THERE,s)
				   in pickler pu v s
				   end),
	    unpickler = (fn (s,hce) =>
			 let val (w,s) = S.getcw s
			 in if w = NOT_THERE then unpickler pu (s,hce)
			    else let val v = lookup w
				 in (v,(s,hce))
				 end
			 end),
	    hasher = hasher pu,
	    eq = eQ pu,
	    typ = TregisterEq(typ pu)}
	end

    fun register s (vs: 'a list) (pu : 'a pu) : 'a pu =
	registerEq
	(eQ pu)
	(fn v => Word.toIntX (#1(hasher pu v (0w0,maxDepth)))) s vs pu

    fun hashConsEq (eq:'a*'a->bool) (pu: 'a pu) : 'a pu =
	let val hash = newHashCount()
	    val (toDyn,fromDyn) =
		Dyn.new (fn v => fn d => #1 (hasher pu v (hash,d))) eq
	in PU
	    {pickler= pickler pu,
	     unpickler= fn is =>
	     let val (v,is) = unpickler pu is
		 val d = HashConsEnv.add (#2 is) (toDyn v)
	     in (fromDyn d, is)
	     end,
	     hasher= hasher pu,
	     eq= eq,
	     typ= typ pu}
	end

    fun hashCons (pu: 'a pu) : 'a pu =
	hashConsEq (eQ pu) pu

    fun nameGen s (pu: 'a pu) : 'a pu =
	let
	in PU
	    {pickler = pickler pu,
	     unpickler = unpickler pu,
	     hasher = hasher pu,
	     eq = eQ pu,
	     typ = Tdecorate(s,typ pu)}
	end

    fun comment str (pu:'a pu) : 'a pu =
	if not comments_p then pu
	else PU
	{pickler = (fn a => fn s =>
		    let val _ = print ("\n[Begin pickling: " ^ str ^ "]\n")
			val s = pickler pu a s
			val _ = print ("\n[End pickling  : " ^ str ^ "]\n")
		    in s
		    end),
	 unpickler = unpickler pu,
	 hasher = hasher pu,
	 eq = eQ pu,
	 typ = typ pu}

    fun checkUnpickle (f: 'a -> unit)
	(pu as PU {pickler,unpickler,eq,typ,hasher} : 'a pu) : 'a pu =
	pu
(*
	PU {pickler=pickler,
	 unpickler=fn is =>
	 let val p as (v,is) = unpickler is
	 in f v ; p
	 end,
	 eq=eq,
	 typ=typ,
	 hasher=hasher}
*)

    fun debugUnpickle (s : string)
	(pu as PU {pickler,unpickler,eq,typ,hasher} : 'a pu) : 'a pu =
	pu
(*
	PU {pickler=pickler,
	 unpickler=fn is =>
	 let val _ = print ("unpickling[" ^ s ^ "] begin...\n")
	     val p = unpickler is
	     val _ = print ("unpickling[" ^ s ^ "] end...\n")
	 in p
	 end,
	 eq=eq,
	 typ=typ,
	 hasher=hasher}
*)
  end
