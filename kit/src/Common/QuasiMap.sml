signature QUASI_DOM =
  sig
    type dom
    type name 
    val name : dom -> name
    val pp : dom -> string
  end

functor QuasiMap(structure IntFinMap : MONO_FINMAP where type dom = int
		 structure QD : QUASI_DOM
	         structure Name : NAME
		   sharing type Name.name = QD.name
		 structure Crash : CRASH
		 structure PP : PRETTYPRINT
		 structure Report : REPORT) : MONO_FINMAP =
  struct

    structure IFM = IntFinMap

    fun die s = Crash.impossible ("Quasimap." ^ s)

    fun key d = Name.key(QD.name d)
    fun rigid d = Name.rigid(QD.name d)
      
    type dom = QD.dom
      
    type 'a imap = 'a IFM.map
    datatype 'a map0 = Rigid of (dom * 'a) imap | Flexible of {matchcount: Name.matchcount, imap: (dom * 'a) imap}
      
    datatype 'a map = M of 'a map0 ref | Empty
      
    val empty = Empty
      
    fun singleton (d,e) = 
      let val nd = QD.name d
      in if Name.rigid nd then M (ref (Rigid(IFM.singleton(Name.key nd,(d,e)))))
	 else M (ref (Flexible {imap = IFM.singleton(Name.key nd,(d,e)), matchcount = Name.current_matchcount()}))
      end

    fun isEmpty Empty = true
      | isEmpty _ = false

    fun imap Empty = NONE
      | imap (M(ref(Rigid imap))) = SOME imap
      | imap (M(ref(Flexible {imap,...}))) = SOME imap

    fun imap' (M(ref(Rigid imap))) = imap
      | imap' (M(ref(Flexible {imap,...}))) = imap
      | imap' Empty = die "imap': impossible"
      
    fun ensure_consistent_imap (imap : (dom * 'a) imap) : {rigid: bool, imap : (dom * 'a) imap} =
      let 
	val (consistent, rigid) = (* Property: rigid => consistent *)
	  IFM.Fold(fn((i,(d,_)),(c,r)) => (c andalso key d = i, r andalso rigid d)) (true,true) imap
	val imap = if consistent then imap
		   else IFM.fold(fn((d,e),im) => IFM.add(key d,(d,e),im)) IFM.empty imap
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

    (* Lookup *)
    fun lookup m d = 
      case imap (ensure_consistent m)
	of SOME im => (case IFM.lookup im (key d)
			 of SOME (_, e) => SOME e
			  | NONE => NONE)
	 | NONE => NONE

    fun add (d, e, m) =
      case ensure_consistent m
	of Empty => singleton(d,e)
	 | M(ref(Rigid imap)) =>
	  let val nd = QD.name d
	      val i = Name.key nd
	  in if Name.rigid nd then mk_rigid(IFM.add(i,(d,e),imap))
	     else mk_flex (IFM.add(i,(d,e),imap))
	  end
	 | M(ref(Flexible{imap,...})) => mk_flex (IFM.add(key d,(d,e),imap))

    fun plus(m1,m2) =
      case (ensure_consistent m1, ensure_consistent m2)
	of (Empty, m) => m
	 | (m, Empty) => m
	 | (M(ref(Rigid im1)), M(ref(Rigid im2))) => mk_rigid (IFM.plus(im1,im2))
	 | (m1,m2) => mk_flex (IFM.plus(imap' m1, imap' m2))

    fun remove (d, m) = 
      case ensure_consistent m
	of Empty => NONE
	 | M(ref(Rigid im)) => 
	  (case IFM.remove(key d, im)
	     of SOME im' => if IFM.isEmpty im' then SOME Empty
			    else SOME(mk_rigid im')
	      | NONE => NONE)
	 | M(ref(Flexible{imap,...})) => 
	     (case IFM.remove(key d, imap)
		of SOME im' => if IFM.isEmpty im' then SOME Empty
			       else SOME(mk_flex im')
		 | NONE => NONE)

    fun dom Empty = []
      | dom m = map #1 (IFM.range (imap' m)) 

    fun range Empty = []
      | range m = map #2 (IFM.range (imap' m)) 

    fun list Empty = []
      | list m = IFM.range (imap' m)

    fun fromList nil = Empty
      | fromList l =
      let val (imap, rigid) = 
	    foldl (fn ((d,e),(im,r)) => (IFM.add(key d, (d,e), im), r andalso rigid d)) (IFM.empty, true) l
      in if rigid then mk_rigid imap
	 else mk_flex imap
      end

    fun composemap f m =
      case list m
	of [] => Empty
	 | l => 
	  let val (imap, rigid) = 
	        foldl (fn ((d,e),(im,r)) => (IFM.add(key d,(d, f e),im), r andalso rigid d)) (IFM.empty, true) l
	  in if rigid then mk_rigid imap
	     else mk_flex imap
	  end

    fun ComposeMap f m =
      case list m
	of [] => Empty
	 | l => 
	  let val (imap, rigid) = 
	        foldl (fn ((d,e),(im,r)) => (IFM.add(key d,(d, f(d, e)),im), r andalso rigid d)) (IFM.empty, true) l
	  in if rigid then mk_rigid imap
	     else mk_flex imap
	  end

    fun fold f b m = foldl f b (range m)    
    fun Fold f b m = foldl f b (list m)
    fun filter f m =
      let val l = list m
	  val l' = List.filter f l
      in fromList l'
      end
    fun addList l m1 =
      let val m2 = fromList l
      in plus(m1,m2)
      end
    fun mergeMap f m1 m2 =
      let fun f' ((d1,e1), (d2,e2)) = 
	     if key d1 = key d2 then (d1, f(e1,e2))
	     else die "mergeMap, f'"
      in case (ensure_consistent m1, ensure_consistent m2)
	   of (Empty, m) => m
	    | (m, Empty) => m
	    | (M(ref(Rigid im1)), M(ref(Rigid im2))) => mk_rigid (IFM.mergeMap f' im1 im2)
	    | (m1,m2) => mk_flex (IFM.mergeMap f' (imap' m1) (imap' m2))
      end

    fun pp_lookup pp [] i = die "pp_lookup; element not there"
      | pp_lookup pp (d::rest) i = if i = key d then pp d else pp_lookup pp rest i

    exception Restrict of string
    fun restrict (pp, m, nil) = Empty
      | restrict (pp, m, l) =
      case ensure_consistent m
	of Empty => raise Restrict "[empty map]"
	 | m => let val im = imap' m
		    val ns = map key l
		    val pp' = pp_lookup pp l 
		    val im' = IFM.restrict (pp', im, ns) handle IFM.Restrict s => raise Restrict s
		    val rigid = IFM.fold (fn ((d,_), r) => r andalso rigid d) true im'
		in if rigid then mk_rigid im'
		   else mk_flex im'
		end

    fun enrich f (m1, m2) =
      case (ensure_consistent m1, ensure_consistent m2)
	of (_, Empty) => true
	 | (Empty, _) => false
	 | (m1, m2) => let val im1 = imap' m1
	                   val im2 = imap' m2
			   fun f' ((_,e1),(_,e2)) = f (e1,e2)  (* This is the function we want to *)
		       in IFM.enrich f' (im1, im2)             (* update for more efficient enrichment *)
		       end                                     (* using refs. *)
      

    type StringTree = PP.StringTree
      
    fun layoutMap {start, eq, sep, finish} pp_dom pp_range m =
      let fun layout_entry (d, r) = PP.NODE{start="",finish="",indent=1,childsep=PP.RIGHT eq,
					    children = [pp_dom d, pp_range r]}
      in PP.NODE{start=start,finish=finish,indent=1,childsep=PP.RIGHT sep,
		 children = map layout_entry (list m)}
      end

    type Report = Report.Report

    fun reportMap f m = Report.flatten(map f (list m))


    (* Pickler *)

    fun pu_map0 (pu_d : dom Pickle.pu) (pu_a : 'a Pickle.pu) : 'a map0 Pickle.pu =
	let open Pickle
	    val pu_m = IFM.pu Pickle.int (pairGen(pu_d,pu_a))
	    fun toInt (Rigid _) = 0
	      | toInt (Flexible _) = 1
	    fun eq (Rigid m1, Rigid m2) = #4 pu_m (m1,m2)
	      | eq (Flexible{matchcount,imap},  Flexible{matchcount=mc2,imap=imap2}) =
		#4 Name.pu_matchcount (matchcount,mc2) andalso #4 pu_m (imap,imap2)
	      | eq _ = false
	    fun funRigid (_ : 'a map0 Pickle.pu) : 'a map0 Pickle.pu =
		(fn m0 => fn spe =>
		 case m0 of 
		     Rigid m => pickler pu_m m spe
		   | _ => die "pu.funRigid.pickle",
		 fn supe => let val (m,supe) = unpickler pu_m supe
			    in (Rigid m,supe)
			    end,
		 fn Rigid m => hasher pu_m m
		  | _ => die "pu.funRigid.hasher",
		 eq)
	    fun funFlexible (_ : 'a map0 Pickle.pu) : 'a map0 Pickle.pu =
		(fn m0 => fn spe =>
		 case m0 of 
		     Flexible {matchcount,imap} => 
			 let val spe = pickler Name.pu_matchcount matchcount spe
			 in pickler pu_m imap spe
			 end
		   | _ => die "pu.funFlexible.pickle",
		 fn supe => let val (mc,supe) = unpickler Name.pu_matchcount supe
				val (imap,supe) = unpickler pu_m supe
			    in (Flexible {matchcount=mc,imap=imap},supe)
			    end,
		 fn Flexible {matchcount,imap} => 
		 hashCombine(hasher Name.pu_matchcount matchcount, 
			     hasher pu_m imap)
		  | _ => die "pu.funFlexible.hasher",
		 eq)
	in dataGen (toInt,eq,[funRigid,funFlexible])
	end

    fun pu pu_d pu_a =
	let open Pickle
	    fun to (SOME v) = M v
	      | to NONE = Empty
	    fun from (M v) = SOME v
	      | from Empty = NONE
	in convert (to,from) (optionGen (ref0Gen (pu_map0 pu_d pu_a)))
	end
  end
