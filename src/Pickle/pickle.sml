(* Generic pickle module
 * Copyright, Martin Elsman 2003-01-07 
 *)

structure Listsort =
  struct
    fun sort ordr xs =
      let 
	fun merge []      ys = ys 
	  | merge xs      [] = xs
	  | merge (x::xs) (y::ys) =
	  if ordr(x, y) <> GREATER then x :: merge xs (y::ys)
	  else y :: merge (x::xs) ys
	fun mergepairs l1  []              k = [l1]
	  | mergepairs l1 (ls as (l2::lr)) k =
	  if k mod 2 = 1 then l1::ls
	  else mergepairs (merge l1 l2) lr (k div 2)
        fun nextrun run []      = (run, [])
          | nextrun run (xs as (x::xr)) =
	  if ordr(x, List.hd run) = LESS then (run, xs)
	  else nextrun (x::run) xr
        fun sorting []      ls r = List.hd(mergepairs [] ls 0)
          | sorting (x::xs) ls r =
	  let val (revrun, tail) = nextrun [x] xs
	  in sorting tail (mergepairs (List.rev revrun) ls (r+1)) (r+1) 
	  end
      in sorting xs [] 0 
      end
  end

structure Pickle :> PICKLE = (* was : *)
  struct
    val sharing_p = true
    val linear_refs_p = true
    val very_safe_p = false
    val debug_p = false
    val comments_p = false

    datatype 'a cache = NoCache | Cached of 'a | Caching
	
    infix ==
    fun a == b : bool = false
(*
	let val a_i:int = Unsafe.cast a
	    val b_i:int = Unsafe.cast b
	in a_i = b_i
	end
*)
    structure S = Stream
    structure H = Polyhash
    structure Dyn = EqHashDyn

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
	    fun loop (n,a) = if n >= sz then a
			     else loop (n+1, hashAddSmall0 (Word.fromInt(Char.ord(String.sub(s,n)))) a)
	in loop (0,acc)
	end

    type dyn = Dyn.dyn

    exception PickleExn
    structure PickleEnv :> 
	sig 
	    type pe
	    val empty : unit -> pe
	    val lookup : pe -> dyn -> S.loc option
	    val insert : pe -> dyn * S.loc -> unit
	    val reportBucket : string -> pe * dyn * string -> unit
	    val bucketSizes : pe -> int list
	end =
    struct
	type pe = (dyn, S.loc) H.hash_table
	fun empty () = H.mkTable (Word.toIntX o Dyn.hash maxDepth, Dyn.eq) (10,PickleExn)
	fun lookup pe d = H.peek pe d
	fun insert pe (d,loc) = H.insert pe (d,loc) 
	fun reportBucket s (pe,d,typ) : unit = 
	    if true then () else
		let val (c,h) = H.peekSameHash pe d
		    val maxBucket = 10
		in if c > maxBucket then print ("** " ^ s ^ ".Bucket > " ^ Int.toString maxBucket 
						^ " (c=" ^ Int.toString c ^ ",h=" 
						^ Int.toString h ^") **: " ^ typ ^ "\n")
		   else ()
		end
	fun bucketSizes pe = H.bucketSizes pe
    end

    structure UnpickleEnv :>
	sig
	    type upe
	    val empty : unit -> upe
	    val lookup : upe -> S.loc -> dyn option
	    val insert : upe -> S.loc * dyn -> unit
	end =
    struct
	type upe = (S.loc, dyn) H.hash_table
	fun empty() = H.mkTable (Word.toIntX, op =) (10,PickleExn)
	fun lookup upe loc = H.peek upe loc
	fun insert upe (l,d) = H.insert upe (l,d)
    end

    structure HashConsEnv :>
	sig
	    type hce
	    val empty : unit -> hce
	    val add : hce -> dyn -> dyn
	end =
    struct
	type hce = (dyn, dyn) H.hash_table
	fun empty() = H.mkTable (Word.toIntX o Dyn.hash maxDepth, Dyn.eq) (10,PickleExn)
	fun add hce d =
	    case H.peek hce d of
		SOME d => d
	      | NONE => (H.insert hce (d,d); d)
    end


    type pe = PickleEnv.pe
    type upe = UnpickleEnv.upe
    type hce = HashConsEnv.hce

    type instream = S.IN S.stream * upe * hce
    type outstream = S.OUT S.stream * pe
    type 'a pickler = 'a -> outstream -> outstream
    type 'a unpickler = instream -> 'a * instream
    type 'a hasher = 'a -> word*int -> word*int    (* int: hash depth *)
    type 'a eq = 'a * 'a -> bool

    type 'a pu = {pickler   : 'a pickler, 
		  unpickler : 'a unpickler,
		  hasher    : 'a hasher,
		  eq        : 'a eq,
		  typ       : string}

    fun pickler (pu:'a pu) = #pickler pu
    fun unpickler (pu:'a pu) = #unpickler pu
    fun hasher (pu:'a pu) = #hasher pu
    fun typ (pu:'a pu) = #typ pu

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
	    in {pickler = (fn v => fn (s,pe) =>
			   let val s = S.outw (c,s)
			   in pickler pu v (s,pe)
			   end),
		unpickler = (fn (s,upe,hce) => 
			     let val (w,s) = S.getw s
			     in if w <> c then fail ("debug.expected " ^ str)
				else unpickler pu (s,upe,hce)
			     end),
		hasher = hasher pu,
		eq = #eq pu,
		typ = "DEBUG(" ^ typ pu ^ ")"}
	    end
    end

    fun wordGen s (toWord:''a->Word32.word, fromWord:Word32.word->''a) : ''a pu =
	debug s (
	{pickler = fn w => fn (s,pe) => (S.outcw (toWord w,s), pe),
	 unpickler = fn (s,upe,hce) => let val (w,s) = S.getcw s
				       in (fromWord w, (s,upe,hce))
				       end,
	 hasher = fn a => hashComb (fn p => hashAdd (w32_to_w(toWord a)) p),
	 eq = op =,
	 typ = s})
	
    val word = wordGen "word" (w_to_w32, w32_to_w)
    val word32 = wordGen "word32" (fn x => x, fn x => x)
    val int = wordGen "int" (Word32.fromInt, Word32.toIntX)
    val int32 = wordGen "int32" (Word32.fromLargeInt o Int32.toLarge, Int32.fromLarge o Word32.toLargeIntX)
	
    val bool = wordGen "bool" (fn true => 0w1 | false => 0w0,
			       fn 0w0 => false | _ => true)
    val char = wordGen "char" (Word32.fromInt o Char.ord, 
			       Char.chr o Word32.toIntX)

    fun shareGen0 (pp: 'a -> string) (pu:'a pu) : 'a pu =
      if not sharing_p then pu else
      debug "shareGen0"
      let val REF = 0w0 and DEF = 0w1           
	  val hash_share = newHashCount()
	  val typ = "SH(" ^ typ pu ^ ")"
          val (toDyn,fromDyn) = Dyn.new (fn v => fn d => #1 (hasher pu v (hash_share,d))) (#eq pu)
	  fun h2 v  =
	      hashComb (fn p => hasher pu v (hashAddSmallNoCount hash_share p))
      in  {pickler = (fn v => fn (s, pe:pe) =>
		      let val d = toDyn v
		      in case PickleEnv.lookup pe d of
			  SOME loc => 
			      let val s = S.outcw(REF,s)
				  val s = S.outw(w_to_w32 loc,s)
			      in (s,pe)
			      end
			| NONE =>
			      let val s = S.outcw(DEF,s)
				  val loc = S.getLoc s
				  val res = pickler pu v (s, pe)    (* do insert after the pickling    *)
			      in case PickleEnv.lookup pe d of      (*  - otherwise there are problems *)
				  SOME _ => res                     (*    with cycles.                 *)
				| NONE =>
				      let (*
					  val (c,h) = H.peekSameHash pe d
					  val maxBucket = 10
					  val _ = if c > maxBucket then print ("** Bucket > " ^ Int.toString maxBucket 
									       ^ " (c=" ^ Int.toString c ^ ",h=" 
									       ^ Int.toString h ^") **: " ^ typ ^ "\n"
									       ^ "** Value = " ^ pp v ^ "\n")
						  else ()
					  *)
				      in PickleEnv.insert pe (d,loc) 
				       ; res
				      end
			      end
		      end),
	   unpickler = (fn (s,upe,hce) =>
			let val (tag,s) = S.getcw s
			in if tag = REF then
			    let val (loc,s) = S.getw s
				val loc' = w32_to_w loc
			    in case UnpickleEnv.lookup upe loc' of
				SOME d => (fromDyn d, (s,upe,hce))
			      | NONE => fail ("shareGen.impossible, loc=" ^ Word32.toString loc ^ ", loc'=" ^ Word.toString loc')
			    end
			   else if tag = DEF then
			       let val loc = S.getLoc s
				   val (v,(s,upe,hce)) = unpickler pu (s,upe,hce)
				   val _ = case UnpickleEnv.lookup upe loc of
				       NONE => ()
				     | SOME _ => fail ("shareGen.Location " ^ Word.toString loc ^ " already there!")
			       in UnpickleEnv.insert upe (loc,toDyn v)
				; (v, (s,upe,hce))
			       end
			   else fail "shareGen.impossible2"
			end),
	   hasher = h2,
	   eq = #eq pu,
	   typ = typ}
      end

    fun shareGen a = shareGen0 (fn _ => "no pp") a

    val string : string pu =
	(shareGen o debug "string")
	{pickler = (fn st => fn (s, pe) =>
		    let val sz = size st
			val s = S.outcw(Word32.fromInt sz, s)
			val s = CharVector.foldl (fn (c,cs) => S.out(c,cs)) s st
		    in (s, pe)
		    end),
	 unpickler = (fn (s,upe,hce) =>
		      let val (sz,s) = S.getcw s
			  val sz = Word32.toInt sz
			  fun read (0,s,acc) = (implode(rev acc), s)
			    | read (n,s,acc) = 
			      let val (c,s) = S.get s
			      in read (n-1, s, c :: acc)
			      end
			  val (st,s) = read (sz,s,nil)
		      in  (st, (s,upe,hce))
		      end),
	 hasher = fn s => hashComb (fn (acc, d) => (stringHash acc s, d-1)),
	 eq = op =,
	 typ = "string"}

    fun pairGen0 (pu1 :'a pu, pu2 :'b pu) : ('a * 'b) pu = 
	let val hash_pair = newHashCount()
	in
	    debug "pair"
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
				 let val p = hashAddSmallNoCount hash_pair (hasher pu1 a p)
				 in hashComb (hasher pu2 b) p
				 end)),
	     eq = fn (p1 as (a1,a2),p2 as (b1,b2)) => p1==p2 orelse #eq pu1 (a1,b1) andalso #eq pu2 (a2,b2),
	     typ = "P(" ^ typ pu1 ^ "," ^ typ pu2 ^ ")"}
	end

    fun pairGen pu = shareGen(pairGen0 pu)

    fun refEqGen (eq: 'a ref * 'a ref -> bool) (v_dummy:'a) (pu:'a pu) : 'a ref pu =
      debug "refEqGen"
      let (*val eq = if very_safe_p then op = else eq *)
	  val REF_LOC = 0w0 and REF_DEF = 0w1           
	  val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
	  val typ = "ref(" ^ typ pu ^ ")"
          val (toDyn,fromDyn) = Dyn.new (fn v => fn d => #1 (href v (0w0,d))) eq
      in  {pickler = (fn r as ref v => fn (s, pe:pe) =>
		      let val d = toDyn r
		      in case PickleEnv.lookup pe d of
			  SOME loc => 
			      let val s = S.outcw(REF_LOC,s)
				  val s = S.outw(w_to_w32 loc,s)
			      in (s,pe)
			      end
			| NONE =>
			      let val _ = PickleEnv.reportBucket "RefEqGen" (pe,d,typ)
				  val s = S.outcw(REF_DEF,s)
				  val loc = S.getLoc s
			      in PickleEnv.insert pe (d,loc)
			       ; pickler pu v (s, pe)
			      end
		      end),
	   unpickler = (fn (s,upe,hce) =>
			let val (tag,s) = S.getcw s
			in if tag = REF_LOC then
			    let val (loc,s) = S.getw s
			    in case UnpickleEnv.lookup upe (w32_to_w loc) of
				SOME d => (fromDyn d, (s,upe,hce))
			      | NONE => fail "ref.impossible"
			    end
			   else (* tag = REF_DEF *)
			       let val loc = S.getLoc s
				   val r = ref v_dummy
				   val _ = UnpickleEnv.insert upe (loc,toDyn r)
				   val (v,(s,upe,hce)) = unpickler pu (s,upe,hce)
			       in r := v ; (r, (s,upe,hce))
			       end
			end),
	   hasher = href,
	   eq = eq,
	   typ = typ}
      end

    fun refGen (v_dummy:'a) (pu:'a pu) : 'a ref pu =
	refEqGen (op =) v_dummy pu 

    fun ref0EqGen (eq: 'a ref * 'a ref ->bool) (pu:'a pu) : 'a ref pu =      
      debug "ref0EqGen"
      let val eq = if very_safe_p then op = else eq
	  val REF_LOC = 0w0 and REF_DEF = 0w1           
	  val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
	  val typ = "ref0(" ^ typ pu ^ ")"
          val (toDyn,fromDyn) = Dyn.new (fn v => fn d => #1 (href v (0w0,d))) eq
      in  {pickler = (fn r as ref v => fn (s, pe:pe) =>
		      let val d = toDyn r
		      in case PickleEnv.lookup pe d of
			  SOME loc => 
			      let val s = S.outcw(REF_LOC,s)
				  val s = S.outw(w_to_w32 loc,s)
			      in (s,pe)
			      end
			| NONE =>
			      let val _ = PickleEnv.reportBucket "Ref0EqGen" (pe,d,typ)
				  val s = S.outcw(REF_DEF,s)
				  val loc = S.getLoc s
			      in PickleEnv.insert pe (d,loc)
			       ; pickler pu v (s, pe)
			      end
		      end),
	   unpickler = (fn (s,upe,hce) =>
			let val (tag,s) = S.getcw s
			in if tag = REF_LOC then
			    let val (loc,s) = S.getw s
			    in case UnpickleEnv.lookup upe (w32_to_w loc) of
				SOME d => (fromDyn d, (s,upe,hce))
			      | NONE => fail "ref.impossible"
			    end
			   else (* tag = REF_DEF *)
			       let val loc = S.getLoc s
				   val (v,(s,upe,hce)) = unpickler pu (s,upe,hce)
				   val r = ref v
			       in UnpickleEnv.insert upe (loc,toDyn r) 
				; (r, (s,upe,hce))
			       end
			end),
	   hasher = href,
	   eq = eq,
	   typ = typ}
      end

    fun ref0Gen (pu:'a pu) : 'a ref pu = 
	ref0EqGen (op =) pu

    fun ref0ShGen (pu:'a pu) : 'a ref pu = 
	if very_safe_p then ref0Gen pu
	else ref0EqGen (fn (ref a,ref b) => #eq pu (a,b)) pu 

    fun refOneGen (pu:'a pu) : 'a ref pu =    (* Only works when sharing is enabled! *)
      if not sharing_p orelse not linear_refs_p then ref0Gen pu
      else
      let val hash_ref = newHashCount()
	  fun href (ref a) = hashComb (fn p => hashAddSmall hash_ref (hasher pu a p))
      in
	  debug "refOneGen"
	  {pickler = fn r as ref v => pickler pu v,
	   unpickler = (fn is => let val (v,is) = unpickler pu is
				 in (ref v, is)
				 end),
	   hasher = href,
	   eq = op =,
	   typ = "ref1(" ^ typ pu ^ ")"}
      end

    fun dataGen (name, toInt: 'a -> int, fs : ('a pu -> 'a pu) list) : 'a pu =
	debug "dataGen"
	let val hash_data = newHashCount()
	    val res : 'a pu option ref = ref NONE
	    val ps : 'a pu Vector.vector cache ref = ref NoCache
	    fun p v (s,pe) =
	      let val i = toInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(getPUPI i) v (s,pe)
	      end
            and up (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(getPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and eq(a1:'a,a2:'a) : bool =
		a1 == a2 orelse
		let val n = toInt a1
		in n = toInt a2 andalso #eq (getPUPI n) (a1,a2)
		end
	    and getPUP() = 
		case !res of
		    NONE => let val typ = name ^ "_" ^ Int.toString (length fs)
				fun pp v = "Con" ^ Int.toString (toInt v)
				val pup = shareGen0 pp {pickler=p,unpickler=up,
							hasher=h,eq=eq,typ=typ}
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

    fun data2Gen (aname, aToInt: 'a -> int, afs : ('a pu * 'b pu -> 'a pu) list,
		  bname, bToInt: 'b -> int, bfs : ('a pu * 'b pu -> 'b pu) list) 
	: 'a pu * 'b pu =
	let val aHashData = newHashCount()
	    val bHashData = newHashCount()
	    val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector cache ref = ref NoCache
	    val bPs : 'b pu Vector.vector cache ref = ref NoCache
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(aGetPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		a1==a2 orelse
		let val n = aToInt a1
		in n = aToInt a2 andalso #eq (aGetPUPI n) (a1,a2)
		end
	    and aGetPUP() = 
		case !aRes of
		    NONE => let val typ = aname ^ "_" ^ Int.toString (length afs)
				fun pp v = "Con" ^ Int.toString (aToInt v)
				val pup = shareGen0 pp {pickler=aP,unpickler=aUp,
							hasher=aH,eq=aEq,typ=typ}
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
	    and bP v (s,pe) =
	      let val i = bToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(bGetPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		b1==b2 orelse
		let val n = bToInt b1
		in n = bToInt b2 andalso #eq (bGetPUPI n) (b1,b2)
		end
	    and bGetPUP() = 
		case !bRes of
		    NONE => let val typ = bname ^ "_" ^ Int.toString (length bfs)
				fun pp v = "Con" ^ Int.toString (bToInt v)
				val pup = shareGen0 pp {pickler=bP,unpickler=bUp,
							hasher=bH,eq=bEq,typ=typ}
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

    fun data3Gen (aname, aToInt: 'a -> int, afs : ('a pu * 'b pu * 'c pu -> 'a pu) list,
		  bname, bToInt: 'b -> int, bfs : ('a pu * 'b pu * 'c pu -> 'b pu) list,
		  cname, cToInt: 'c -> int, cfs : ('a pu * 'b pu * 'c pu -> 'c pu) list) 
	: 'a pu * 'b pu * 'c pu =
	let val aHashData = newHashCount()
	    val bHashData = newHashCount()
	    val cHashData = newHashCount()
	    val aRes : 'a pu option ref = ref NONE
	    val bRes : 'b pu option ref = ref NONE
	    val cRes : 'c pu option ref = ref NONE
	    val aPs : 'a pu Vector.vector cache ref = ref NoCache
	    val bPs : 'b pu Vector.vector cache ref = ref NoCache
	    val cPs : 'c pu Vector.vector cache ref = ref NoCache
	    fun aP v (s,pe) =
	      let val i = aToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(aGetPUPI i) v (s,pe)
	      end
            and aUp (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(aGetPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and aEq(a1:'a,a2:'a) : bool =
		a1==a2 orelse
		let val n = aToInt a1
		in n = aToInt a2 andalso #eq (aGetPUPI n) (a1,a2)
		end
	    and aGetPUP() = 
		case !aRes of
		    NONE => let val typ = aname ^ "_" ^ Int.toString (length afs)
				fun pp v = "Con" ^ Int.toString (aToInt v)
				val pup = shareGen0 pp {pickler=aP,unpickler=aUp,
							hasher=aH,eq=aEq,typ=typ}
			    in aRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and aGetPUPI (i:int) =
		case !aPs of
		    NoCache => let val _ = aPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) afs
				   val psv = Vector.fromList ps0
			       in aPs := Cached psv
				   ; Vector.sub(psv,i)
			       end 
		  | Cached psv => Vector.sub(psv,i)		
		  | Caching => fail ("dataGen3.Caching.a: " ^ aname)
	    and bP v (s,pe) =
	      let val i = bToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(bGetPUPI i) v (s,pe)
	      end
            and bUp (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(bGetPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and bEq(b1:'b,b2:'b) : bool =
		b1==b2 orelse
		let val n = bToInt b1
		in n = bToInt b2 andalso #eq (bGetPUPI n) (b1,b2)
		end
	    and bGetPUP() = 
		case !bRes of
		    NONE => let val typ = bname ^ "_" ^ Int.toString (length bfs)
				fun pp v = "Con" ^ Int.toString (bToInt v)
				val pup = shareGen0 pp {pickler=bP,unpickler=bUp,
							hasher=bH,eq=bEq,typ=typ}
			    in bRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and bGetPUPI (i:int) =
		case !bPs of
		    NoCache => let val _ = bPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) bfs
				   val psv = Vector.fromList ps0
			       in bPs := Cached psv
				   ; Vector.sub(psv,i)
			       end 
		  | Cached psv => Vector.sub(psv,i)
		  | Caching => fail ("dataGen3.Caching.b: " ^ bname)
	    and cP v (s,pe) =
	      let val i = cToInt v
		  val s = S.outcw (Word32.fromInt i, s)
	      in pickler(cGetPUPI i) v (s,pe)
	      end
            and cUp (s,upe,hce) =
	      let val (w,s) = S.getcw s
	      in unpickler(cGetPUPI (Word32.toInt w)) (s,upe,hce)
	      end
	    and cEq(c1:'c,c2:'c) : bool =
		c1==c2 orelse
		let val n = cToInt c1
		in n = cToInt c2 andalso #eq (cGetPUPI n) (c1,c2)
		end
	    and cGetPUP() = 
		case !cRes of
		    NONE => let val typ = cname ^ "_" ^ Int.toString (length cfs)
				fun pp v = "Con" ^ Int.toString (cToInt v)
				val pup = shareGen0 pp {pickler=cP,unpickler=cUp,
							hasher=cH,eq=cEq,typ=typ}
			    in cRes := SOME pup
			     ; pup
			    end
		  | SOME pup => pup
	    and cGetPUPI (i:int) =
		case !cPs of
		    NoCache => let val _ = cPs := Caching
				   val ps0 = map (fn f => f (aGetPUP(),bGetPUP(),cGetPUP())) cfs
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
        in (debug "data3Gen.a" (aGetPUP()), debug "data3Gen.b" (bGetPUP()), debug "data3Gen.c" (cGetPUP()))
	end

    fun con0 (b: 'b) (_: 'a) =
	{pickler = fn _ => fn spe => spe,
	 unpickler = fn is => (b,is),
	 hasher = fn _ => fn p => p,
	 eq = fn _ => true,                  (* tag is checked with toInt in dataNGen *)
	 typ = "con0"}               

    fun con1 (con:'a->'b) (decon: 'b->'a) (pu: 'a pu) =
	{pickler = fn b:'b => pickler pu (decon b),
	 unpickler = (fn is => 
		      let val (a,is) = unpickler pu is
		      in (con a,is)
		      end),
	 hasher = fn b:'b => hashComb (fn p => hasher pu (decon b) p),
	 eq = fn (b1:'b, b2:'b) => #eq pu (decon b1, decon b2),
	 typ = "con1"}

    fun newHash (f: 'a -> int) (pu: 'a pu) : 'a pu =
	{pickler= pickler pu,
	 unpickler = unpickler pu,
	 hasher = hashComb o hashAdd o Word.fromInt o f,
	 eq= #eq pu,
	 typ = typ pu}

    fun maybeNewHash (f: 'a -> int option) (pu: 'a pu) : 'a pu =
	{pickler= pickler pu,
	 unpickler = unpickler pu,
	 hasher = fn a => hashComb (fn p => 
				    case f a of
					SOME i => hashAdd (Word.fromInt i) p
				      | NONE => hasher pu a p),
	 eq= #eq pu,
	 typ = typ pu}

    fun combHash (f: 'a -> int) (pu: 'a pu) : 'a pu =
	{pickler= pickler pu,
	 unpickler = unpickler pu,
	 hasher = fn a:'a => hashComb (fn p =>
				       let val p = hashAdd (Word.fromInt(f a)) p
				       in hashComb (hasher pu a) p
				       end),
	 eq= #eq pu,
	 typ = typ pu}

    fun listGen (pu_a: 'a pu) : 'a list pu =
	(debug "list" o combHash length)
	let fun toInt nil = 0
	      | toInt (op :: _) = 1
	    val f_nil = con0 nil
	    fun f_cons pu =
		con1 (op ::) (fn op :: p => p | _ => fail "cons")
		(pairGen0(pu_a,pu))
	in dataGen ("(" ^ typ pu_a ^ ")list",toInt,[f_nil,f_cons])
	end

    fun optionGen (pu_a: 'a pu) : 'a option pu =
	debug "option"
	let fun toInt NONE = 0
	      | toInt (SOME _) = 1
	    val fun_NONE = con0 NONE
	    fun fun_SOME _ = 
		con1 SOME (fn SOME v => v | NONE => fail "option") pu_a
	in dataGen("(" ^ typ pu_a ^ ")option",toInt,[fun_NONE,fun_SOME])
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
	in
	    {pickler = fn v => fn (s,pe) => (S.outcw(w_to_w32(lookupw wxs v),s),pe),
	     unpickler = (fn (s,upe,hce) =>
			  let val (w,s) = S.getcw s
			  in (lookupv wxs (w32_to_w w), (s,upe,hce))
			  end),
	     hasher = (fn v => hashComb (fn p => hashAddSmallNoCount hash_enum
					 (hashAddSmallNoCount (lookupw wxs v) p))),
	     eq = op =,
	     typ = "enum(" ^ name ^ "," ^ Int.toString (length xs) ^ ")"}
	end

    fun fromString (s : string) : instream  = 
	(S.openIn s, UnpickleEnv.empty(), HashConsEnv.empty())

    fun fromStringHashCons ((_,_,hce) : instream) (s : string) : instream  =
	(S.openIn s, UnpickleEnv.empty(), hce)

    val ptest = false
    fun toString ((os,pe):outstream) : string = 
	let val res = S.toString os
	    val _ = if not ptest then () else
		    let
			val l = PickleEnv.bucketSizes pe
			val l = Listsort.sort Int.compare l
			val _ = print ("Buckets: " ^ Int.toString (length l) ^ "\n")
			val l = List.take(l,10) handle _ => l
			val _ = app (fn i => print ("  " ^ Int.toString i)) l
		    in ()
		    end
	in res
	end
	
    fun empty() : outstream = 
	(S.openOut(), 
	 PickleEnv.empty())

    fun convert0 (to: 'a->'b ,back: 'b->'a) (pu:'a pu) : 'b pu =
	let val hash_conv = newHashCount()
	in
	    {pickler = fn v => fn s => pickler pu (back v) s,
	     unpickler = (fn is => let val (v,is) = unpickler pu is
				   in (to v,is)
				   end),
	     hasher = fn v => hashComb (fn p => hashAddSmallNoCount hash_conv ((hasher pu o back) v p)),
	     eq = fn (x,y) => #eq pu (back x, back y),
	     typ = "conv(" ^ typ pu ^ ")"}
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

    val real = 
	debug "real"
	(convert (fn v => 
		  case Real.fromString v of
		      SOME v => v
		    | NONE => fail "real.convert",
		  Real.toString) 
	 string)

    val time = 
	debug "time" (convert (Time.fromReal,Time.toReal) real)

    val unit : unit pu =
	{pickler = fn () => fn os => os,
	 unpickler = fn is => ((),is),
	 hasher = fn () => hashComb (fn p => hashAddSmallNoCount UNIT_HASH p),
	 eq = fn _ => true,
	 typ = "unit"}

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

    fun registerEq (eq: 'a * 'a -> bool) (key : 'a -> int) (debug_str:string) (vs: 'a list) (pu : 'a pu) : 'a pu =
	let val h : ('a,word) H.hash_table = H.mkTable (key, eq) (10,PickleExn)
(*	    val _ = print ("registerEq: " ^ debug_str ^ "\n") *)
	    val _ = List.foldl (fn (e,n) => (H.insert h (e,n); n + 0w1)) 0w1 vs
	    val v = Vector.fromList vs
	    fun lookup w = 
		let val i = Word.toInt w - 1
		in Vector.sub(v,i)
		end		    
	    val NOT_THERE : word = 0w0
	in {pickler = (fn v => fn (s,pe) => 
		       case H.peek h v of
			   SOME w => (S.outcw(w_to_w32 w,s),pe)
			 | NONE => let val s = S.outcw(w_to_w32 NOT_THERE,s)
				   in pickler pu v (s,pe)
				   end),
	    unpickler = (fn (s,upe,hce) =>
			 let val (w,s) = S.getcw s
			     val w = w32_to_w w
			 in if w = NOT_THERE then unpickler pu (s,upe,hce)
			    else let val v = lookup w
				 in (v,(s,upe,hce))
				 end
			 end),
	    hasher = hasher pu,
	    eq = #eq pu,
	    typ = "RegisterEq(" ^ typ pu ^ ")"}
	end

    fun register s (vs: 'a list) (pu : 'a pu) : 'a pu =
	registerEq 
	(#eq pu) 
	(fn v => Word.toIntX (#1(hasher pu v (0w0,maxDepth)))) s vs pu

    fun hashConsEq (eq:'a*'a->bool) (pu: 'a pu) : 'a pu =
	let val hash = newHashCount()
	    val (toDyn,fromDyn) = Dyn.new (fn v => fn d => #1 (hasher pu v (hash,d))) eq
	in  {pickler= pickler pu,
	     unpickler= fn is => 
	     let val (v,is) = unpickler pu is
		 val d = HashConsEnv.add (#3 is) (toDyn v)
	     in (fromDyn d, is)
	     end,
	     hasher= hasher pu,
	     eq= eq,
	     typ= typ pu}
	end

    fun hashCons (pu: 'a pu) : 'a pu = 
	hashConsEq (#eq pu) pu
	 
    fun nameGen s (pu: 'a pu) : 'a pu =
	let fun decorate s = "(" ^ s ^ " = " ^ typ pu ^ ")"
	in  {pickler = pickler pu,
	     unpickler = unpickler pu,
	     hasher = hasher pu,
	     eq = #eq pu,
	     typ = decorate s}
	end

    fun comment s (pu:'a pu) : 'a pu =
	if not comments_p then pu
	else
	{pickler = (fn a => fn spe => 
		    let val pos = (S.getLoc o #1) spe
			val _ = print ("\n[Begin pickling: " ^ s ^ " - pos=" ^ (Int.toString o Word.toInt) pos ^ "]\n") 
			val spe = pickler pu a spe
			val pos' = (S.getLoc o #1) spe
			val _ = print ("\n[End pickling  : " ^ s ^ " - pos=" ^ (Int.toString o Word.toInt) pos' ^ ", diff=" ^
				       Int.toString(Word.toInt pos' - Word.toInt pos) ^ "]\n") 
		    in spe
		    end),
	 unpickler = unpickler pu,
	 hasher = hasher pu,
	 eq = #eq pu,
	 typ = typ pu}

    fun checkUnpickle (f: 'a -> unit) (pu as {pickler,unpickler,eq,typ,hasher} : 'a pu) : 'a pu =
	pu
(*
	{pickler=pickler,
	 unpickler=fn is => 
	 let val p as (v,is) = unpickler is
	 in f v ; p
	 end,
	 eq=eq,
	 typ=typ,
	 hasher=hasher} 
*)

    fun debugUnpickle (s : string) (pu as {pickler,unpickler,eq,typ,hasher} : 'a pu) : 'a pu =
	pu
(*
	{pickler=pickler,
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

