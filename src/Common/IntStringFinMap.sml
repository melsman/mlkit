(* Finite maps with domain Int * String implemented with
 * patricia trees (for efficient concatenation) and AVL
 * trees for distinguishing entries with the same integer values. *)

structure IntStringFinMap: MONO_FINMAP (*where type dom = int * string *) =
  struct
      structure PP = PrettyPrint
      structure IM = IntFinMap
      structure SM = StringFinMap

      fun die s = Crash.impossible ("IntStringFinMap." ^ s)

      type dom = int * string

      type 'b map = 'b SM.map IM.map

      fun fold f a im =
	  IM.fold (fn (sm,a) => SM.fold f a sm) a im

      fun Fold f a im =
	  IM.Fold (fn ((i,sm),a) => SM.Fold (fn ((s,v),a) => f(((i,s),v),a)) a sm) a im

      fun dom m = Fold (fn ((d,_),a) => d::a) nil m

      val empty : 'b map = IM.empty

      fun singleton ((i,s):dom, e) = IM.singleton(i,SM.singleton(s,e))

      val isEmpty = IM.isEmpty

      fun lookup m (i,s) =
	  case IM.lookup m i of
	      NONE => NONE
	    | SOME m => SM.lookup m s

      val check_p = false

      fun checkOne s d m =
	  if not(check_p) then m
	  else m before
	  (case lookup m d of
	       SOME _ => ()
	     | NONE => die (s ^ " - Failed to find: " ^ Int.toString (#1 d) ^ "(" ^ #2 d ^ ")"))

      fun checkDom s m0 m =
	  if not(check_p) then m
	  else
	  m before
	  (app (fn d => (checkOne s d m; ())) (dom m0))

      fun checkDom' s d m =
	  if not(check_p) then m
	  else
	  m before
	  (app (fn d => (checkOne s d m; ())) d)

      fun checkAll s m =
	  if not(check_p) then m
	  else checkDom s m m

      fun checkAllOpt s m =
	  case m of NONE => m
	| SOME m' => m before (checkAll s m'; ())

      fun add ((i,s):dom,v,im) = checkOne "add" (i,s)
	  (case IM.lookup im i of
	       NONE => IM.add(i,SM.singleton(s,v),im)
	     | SOME sm => IM.add(i,SM.add(s,v,sm),im))

      fun plus (im1,im2) =
	  checkDom "plus1" im1
	  (checkDom "plus2" im2
	   (checkAll "plus3"
	    (IM.mergeMap SM.plus im1 im2)))

      fun remove ((i,s),im) =
	  checkAllOpt "remove"
	  (case IM.lookup im i of
	       NONE => NONE
	     | SOME sm =>
		  case SM.remove (s,sm) of
		      NONE => NONE
		    | SOME sm => SOME (IM.add (i,sm,im)))

      fun range m = Fold (fn ((_,r),a) => r::a) nil m

      fun composemap f m = IM.composemap (SM.composemap f) m

      fun ComposeMap f m = IM.ComposeMap (fn (i,sm) =>
					  SM.ComposeMap (fn (s,v) => f((i,s),v)) sm) m

      fun list m = Fold (op ::) nil m

      fun filter (f:dom * 'b -> bool) (t:'b map) =
	  Fold (fn ((k,d), t') => if f (k,d) then add(k,d,t')
				  else t') empty t

      fun addList [] (t : 'b map) : 'b map = t
	| addList ((k : dom, d : 'b) :: rest) t =
	  addList rest (add (k, d, t))

      fun fromList l = addList l empty

      fun mergeMap f im1 im2 =
	  IM.mergeMap (fn (sm1,sm2) =>
		       SM.mergeMap f sm1 sm2) im1 im2

      exception Restrict of string
      fun restrict(pp, m: 'b map, dom : dom list) : 'b map =
	  let val m = checkAll "restrict1" m
	  in
	      checkDom' "restrict" dom
	      (foldl(fn (d, acc) =>
		     case lookup m d of
			 SOME res => add(d,res,acc)
		       | NONE => raise Restrict(pp d)) empty dom)
	  end

      fun enrich en (m0, m) =
	  Fold(fn ((d,r),b) => b andalso
	       case lookup m0 d of
		   SOME r0 => en(r0,r)
		 | NONE => false) true m

      type StringTree = PP.StringTree

      fun layoutMap {start, eq=equal, sep, finish}
	  layoutDom layoutRan m =
	  PP.NODE {start=start,
		   finish=finish,
		   children=map (fn (d,r) =>
				 PP.NODE {start="",
					  finish="",
					  children=[layoutDom d,
						    layoutRan r],
					  indent=3,
					  childsep=PP.RIGHT equal})
		   (list m),
		   indent=3,
		   childsep=PP.RIGHT sep}

      type Report = Report.Report

      fun reportMap f t = Report.flatten(map f (list t))

      val reportMapSORTED  = reportMap

      (* Pickler - because the pickle library does not support pickle deconstructors,
       * the pu_dom pickler cannot be used! Instead, we hardwire the int and string
       * picklers, which wouldn't work in general! *)
      fun pu (pu_dom : dom Pickle.pu) (pu_r : 'a Pickle.pu) : 'a map Pickle.pu =
	  IM.pu Pickle.int (SM.pu Pickle.string pu_r)

      fun puNoShare (pu_dom : dom Pickle.pu) (pu_r : 'a Pickle.pu) : 'a map Pickle.pu =
	  IM.puNoShare Pickle.int (SM.puNoShare Pickle.string pu_r)

  end
