signature QUASI_DOM =
  sig
    type dom
    type name 
    val name : dom -> name
    val pp : dom -> string
  end

functor QuasiMap(structure IntStringFinMap : MONO_FINMAP where type dom = int * string
  	         structure QD : QUASI_DOM
	         structure Name : NAME
		   sharing type Name.name = QD.name
		 structure Crash : CRASH
		 structure PP : PRETTYPRINT
		 structure Report : REPORT) : MONO_FINMAP =
  struct

    structure M = IntStringFinMap

    fun die s = Crash.impossible ("QuasiMap2." ^ s)

    fun key d = Name.key(QD.name d)
    fun rigid d = Name.rigid(QD.name d)
      
    type dom = QD.dom
      
    type 'a imap = 'a M.map
    datatype 'a map0 = Rigid of (dom * 'a) imap 
                     | Flexible of {matchcount: Name.matchcount, 
				    imap: (dom * 'a) imap}
      
    datatype 'a map = M of 'a map0 ref | Empty
      
    val empty = Empty
      
    fun singleton (d,e) = 
      let val nd = QD.name d
	  val k = Name.key nd
      in if Name.rigid nd then M (ref (Rigid(M.singleton(k,(d,e)))))
	 else M (ref (Flexible {imap = M.singleton(k,(d,e)), 
				matchcount = Name.current_matchcount()}))
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
	  M.Fold(fn((i,(d,_)),(c,r)) => (c andalso key d = i, r andalso rigid d)) (true,true) imap
	val imap = if consistent then imap
		   else M.fold(fn((d,e),im) => M.add(key d,(d,e),im)) M.empty imap
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
	of SOME im => (case M.lookup im (key d)
			 of SOME (_, e) => SOME e
			  | NONE => NONE)
	 | NONE => NONE

    fun add (d, e, m) =
      case ensure_consistent m
	of Empty => singleton(d,e)
	 | M(ref(Rigid imap)) =>
	  let val nd = QD.name d
	      val i = Name.key nd
	  in if Name.rigid nd then mk_rigid(M.add(i,(d,e),imap))
	     else mk_flex (M.add(i,(d,e),imap))
	  end
	 | M(ref(Flexible{imap,...})) => mk_flex (M.add(key d,(d,e),imap))

    fun plus(m1,m2) =
      case (ensure_consistent m1, ensure_consistent m2)
	of (Empty, m) => m
	 | (m, Empty) => m
	 | (M(ref(Rigid im1)), M(ref(Rigid im2))) => mk_rigid (M.plus(im1,im2))
	 | (m1,m2) => mk_flex (M.plus(imap' m1, imap' m2))

    fun remove (d, m) = 
      case ensure_consistent m
	of Empty => NONE
	 | M(ref(Rigid im)) => 
	  (case M.remove(key d, im)
	     of SOME im' => if M.isEmpty im' then SOME Empty
			    else SOME(mk_rigid im')
	      | NONE => NONE)
	 | M(ref(Flexible{imap,...})) => 
	     (case M.remove(key d, imap)
		of SOME im' => if M.isEmpty im' then SOME Empty
			       else SOME(mk_flex im')
		 | NONE => NONE)

    fun dom Empty = []
      | dom m = map #1 (M.range (imap' m)) 

    fun range Empty = []
      | range m = map #2 (M.range (imap' m)) 

    fun list Empty = []
      | list m = M.range (imap' m)

    fun fromList nil = Empty
      | fromList l =
      let val (imap, rigid) = 
	    foldl (fn ((d,e),(im,r)) => (M.add(key d, (d,e), im), r andalso rigid d)) (M.empty, true) l
      in if rigid then mk_rigid imap
	 else mk_flex imap
      end

    fun composemap f m =
      case list m
	of [] => Empty
	 | l => 
	  let val (imap, rigid) = 
	        foldl (fn ((d,e),(im,r)) => (M.add(key d,(d, f e),im), r andalso rigid d)) (M.empty, true) l
	  in if rigid then mk_rigid imap
	     else mk_flex imap
	  end

    fun ComposeMap f m =
      case list m
	of [] => Empty
	 | l => 
	  let val (imap, rigid) = 
	        foldl (fn ((d,e),(im,r)) => (M.add(key d,(d, f(d, e)),im), r andalso rigid d)) (M.empty, true) l
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
	    | (M(ref(Rigid im1)), M(ref(Rigid im2))) => mk_rigid (M.mergeMap f' im1 im2)
	    | (m1,m2) => mk_flex (M.mergeMap f' (imap' m1) (imap' m2))
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
		    val im' = M.restrict (pp', im, ns) handle M.Restrict s => raise Restrict s
		    val rigid = M.fold (fn ((d,_), r) => r andalso rigid d) true im'
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
		       in M.enrich f' (im1, im2)             (* update for more efficient enrichment *)
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
	    fun szMap m = M.fold (fn (_,a) => a+1) 0 m
	    val pu_m = combHash szMap (M.pu (pairGen(Pickle.int,Pickle.string)) (pairGen(pu_d,pu_a)))
	    fun toInt (Rigid _) = 0
	      | toInt (Flexible _) = 1
	    fun fun_Rigid _ =
		con1 Rigid (fn Rigid a => a | _ => die "pu_map0.Rigid")
		pu_m
	    fun fun_Flexible _ =
		con1 (fn (c,m) => Flexible{matchcount=c,imap=m}) 
		(fn Flexible{matchcount=c,imap=m} => (c,m)
	          | _ => die "pu_map0.Flexible")
		(pairGen0(Name.pu_matchcount,pu_m))
	in dataGen ("QuasiMap.map0",toInt,[fun_Rigid,fun_Flexible])
	end

    fun pu pu_d pu_a =
	let open Pickle
	    fun to (SOME v) = M v
	      | to NONE = Empty
	    fun from (m as M v) = if M.isEmpty (imap' m) then die "pu.hmmm" else SOME v
	      | from Empty = NONE
	in convert (to,from) (optionGen (ref0ShGen (pu_map0 pu_d pu_a)))
	end
  end
