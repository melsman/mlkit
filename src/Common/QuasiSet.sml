functor QuasiSet(QD : QUASI_DOM) : KIT_MONO_SET =
  struct
    structure PP = PrettyPrint
    structure M = IntStringFinMap

    fun die s = Crash.impossible ("QuasiSet." ^ s)

    fun key d = Name.key(QD.name d)
    fun rigid d = Name.rigid(QD.name d)

    type elt = QD.dom

    type 'a imap = 'a M.map
    datatype map0 = Rigid of elt imap | Flexible of {matchcount: Name.matchcount, imap: elt imap}

    datatype Set = M of map0 ref | Empty

    fun getOne _ = die "getOne: not implemented"

    val empty = Empty

    fun singleton d =
      let val nd = QD.name d
      in if Name.rigid nd then M (ref (Rigid(M.singleton(Name.key nd,d))))
	 else M (ref (Flexible {imap = M.singleton(Name.key nd,d), matchcount = Name.current_matchcount()}))
      end

    fun isEmpty Empty = true
      | isEmpty _ = false

    fun imap Empty = NONE
      | imap (M(ref(Rigid imap))) = SOME imap
      | imap (M(ref(Flexible {imap,...}))) = SOME imap

    fun imap' (M(ref(Rigid imap))) = imap
      | imap' (M(ref(Flexible {imap,...}))) = imap
      | imap' Empty = die "imap': impossible"

    fun ensure_consistent_imap (imap : elt imap) : {rigid: bool, imap : elt imap} =
      let
	val (consistent, rigid) = (* Property: rigid => consistent *)
	  M.Fold(fn((i,d),(c,r)) => (c andalso key d = i, r andalso rigid d)) (true,true) imap
	val imap = if consistent then imap
		   else M.fold(fn(d,im) => M.add(key d,d,im)) M.empty imap
      in {rigid=rigid,imap=imap}
      end

    (* Operation for ensuring consistency of integer map *)
    fun ensure_consistent (m as Empty) = m
      | ensure_consistent (m as M(ref(Rigid _))) = m
      | ensure_consistent (m as M(r as ref(Flexible{imap,matchcount}))) =
      if Name.matchcount_lt (matchcount, Name.current_matchcount()) then
	let val {rigid, imap} = ensure_consistent_imap imap
	in if rigid then (r := Rigid imap; m)
	   else (r := Flexible{imap=imap,matchcount=Name.current_matchcount()}; m)
	end
      else m

    fun mk_flex imap = M(ref(Flexible{imap=imap,matchcount=Name.current_matchcount()}))
    fun mk_rigid imap = M(ref(Rigid imap))

    fun member d m =
      case imap (ensure_consistent m)
	of SOME im => (case M.lookup im (key d)
			 of SOME _ => true
			  | NONE => false)
	 | NONE => false

    fun insert d m =
      case ensure_consistent m
	of Empty => singleton d
	 | M(ref(Rigid imap)) =>
	  let val nd = QD.name d
	      val i = Name.key nd
	  in if Name.rigid nd then mk_rigid(M.add(i,d,imap))
	     else mk_flex (M.add(i,d,imap))
	  end
	 | M(ref(Flexible{imap,...})) => mk_flex (M.add(key d,d,imap))

    fun union m1 m2 =
      case (ensure_consistent m1, ensure_consistent m2)
	of (Empty, m) => m
	 | (m, Empty) => m
	 | (M(ref(Rigid im1)), M(ref(Rigid im2))) => mk_rigid (M.plus(im1,im2))
	 | (m1,m2) => mk_flex (M.plus(imap' m1, imap' m2))

    fun remove d m =
      let val m = ensure_consistent m
      in case m
	   of Empty => m
	    | M(ref(Rigid im)) =>
	     (case M.remove(key d, im)
		of SOME im' => if M.isEmpty im' then Empty
			       else mk_rigid im'
		 | NONE => m)
	    | M(ref(Flexible{imap,...})) =>
		(case M.remove(key d, imap)
		   of SOME im' => if M.isEmpty im' then Empty
				  else mk_flex im'
		    | NONE => m)
      end

    fun mk (im,r) = if M.isEmpty im then Empty
		    else if r then mk_rigid im
			 else mk_flex im

    fun difference m1 m2 =
      case (ensure_consistent m1,ensure_consistent m2)
	of (Empty,_) => Empty
	 | (m1,Empty) => m1
	 | (m1,m2) =>
	  let val im1 = imap' m1
	      val im2 = imap' m2
	      val (im,rigid) =
		M.Fold(fn((i1,d1),(im,r)) =>
			 case M.lookup im2 i1
			   of SOME _ => (im,r)
			    | NONE => (M.add(i1,d1,im),r andalso rigid d1)) (M.empty,true) im1
	  in mk(im,rigid)
	  end

    fun intersect m1 m2 =
      case (ensure_consistent m1,ensure_consistent m2)
	of (Empty,_) => Empty
	 | (_,Empty) => Empty
	 | (m1,m2) =>
	  let val im1 = imap' m1
	      val im2 = imap' m2
	      val (im,rigid) =
		M.Fold(fn((i1,d1),(im,r)) =>
			 case M.lookup im2 i1
			   of SOME _ => (M.add(i1,d1,im),r andalso rigid d1)
			    | NONE => (im,r)) (M.empty,true) im1
	  in mk(im,rigid)
	  end

    fun partition f m =
      case ensure_consistent m
	of Empty => (Empty, Empty)
	 | m => let val im = imap' m
		    val (im1,r1,im2,r2) =
			 M.Fold(fn((i,d),(im1,r1,im2,r2)) =>
				  if f d then (M.add(i,d,im1),r1 andalso rigid d,im2,r2)
				  else (im1,r1,M.add(i,d,im2),r2 andalso rigid d)) (M.empty,true,M.empty,true) im
		in (mk(im1,r1), mk(im2,r2))
		end

    fun subst (a,b) m =
      case ensure_consistent m
	of Empty => Empty
	 | m => let val im = imap' m
		    val keya = key a
		    val (im,r) = M.Fold(fn((i,d),(im,r)) =>
					  if keya = i then (M.add(key b,b,im),r andalso rigid b)
					  else (M.add(i,d,im), r andalso rigid d)) (M.empty,true) im
		in mk(im,r)
		end

    fun fold f b m =
      case ensure_consistent m
	of Empty => b
	 | m => let val im = imap' m
		in M.fold (fn (a1,a2) => f a1 a2) b im
		end

    fun map f m =
      case ensure_consistent m
	of Empty => m
	 | m => let val im = imap' m
		    val (im,r) = M.fold(fn(d,(im,r)) =>
					  let val d' = f d
					  in (M.add(key d',d',im),r andalso rigid d')
					  end) (M.empty,true) im
		in mk(im,r)
		end

    fun apply f m = fold (fn d => fn () => f d) () m

    fun list Empty = []
      | list m = M.range (imap' (ensure_consistent m))

    fun fromList nil = Empty
      | fromList l =
      let val (im,r) = foldl (fn (d,(im,r)) => (M.add(key d,d,im), r andalso rigid d)) (M.empty, true) l
      in mk (im,r)
      end

    fun addList l m1 =
      let val m2 = fromList l
      in union m1 m2
      end

    fun size m =
      case imap(ensure_consistent m)
	of NONE => 0
	 | SOME im => M.fold(fn(_,n)=>n+1) 0 im

    fun eq (s1: Set) (s2: Set) : bool =
      let fun f s = fold(fn d => fn b => b andalso member d s2) true s
      in size s1 = size s2 andalso f s1
      end


    type StringTree = PP.StringTree

    fun layoutSet {start, sep, finish} pp_elt m =
      PP.NODE{start=start,finish=finish,indent=1,childsep=PP.RIGHT sep,
	      children = List.map pp_elt (list m)}

    fun pu_map0 pu_e : map0 Pickle.pu =
	let val pu_m = M.pu (Pickle.pairGen(Pickle.int,Pickle.string)) pu_e
	    fun toInt (Rigid _) = 0
	      | toInt (Flexible _) = 1
	    fun fun_Rigid _ =
		Pickle.con1 Rigid (fn Rigid a => a | _ => die "pu_map0.Rigid")
		pu_m
	    fun fun_Flexible _ =
		Pickle.con1 (fn (c,m) => Flexible{matchcount=Name.matchcount_invalid,imap=m})  (* invalidate earlier matchcount; i.e., force
											 * ensurance of consistency *)
		(fn Flexible{matchcount=c,imap=m} => (c,m) | _ => die "pu_map0.Flexible")
		(Pickle.pairGen(Name.pu_matchcount,pu_m))
	in Pickle.dataGen ("QuasiSet.map0",toInt,[fun_Rigid,fun_Flexible])
	end

    fun pu pu_e =
	let fun to (SOME v) = M v
	      | to NONE = Empty
	    fun from (M v) = SOME v
	      | from Empty = NONE
	in Pickle.convert (to,from) (Pickle.optionGen (Pickle.ref0Gen (pu_map0 pu_e)))
	end

  end
