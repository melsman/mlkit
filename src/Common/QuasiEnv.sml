(* Environment with a persistent part to be used accross program units
 * ('a map) and a combined persistent/non-persistent (hashing) map to
 * be used locally for each program unit ('a qmap).  
 *)

functor QuasiEnv(structure OFinMap : MONO_FINMAP where type StringTree = PrettyPrint.StringTree
		 val key : OFinMap.dom -> int
		 val eq  : OFinMap.dom * OFinMap.dom -> bool) : QUASI_ENV =
  struct
    structure PP = PrettyPrint
    structure H = Polyhash
    structure Env = OFinMap

    fun die s = Crash.impossible ("QuasiEnv." ^ s)

    type 'a map = 'a Env.map
    type dom = Env.dom
    type 'a qmap = 'a map * (dom,'a) H.hash_table

    val exn = Fail "QuasiEnv"
    fun mk (n : int) (m : 'a map) : 'a qmap = (m, H.mkTable (key,eq) (n,exn))

    fun lookup ((m,h) : '_a qmap) (a : dom) : '_a option =           (************************)
      case H.peek h a of                                             (* First, lookup in the *)
	  r as SOME _ => r                                           (* hash table.          *)
	 | NONE => Env.lookup m a                                    (************************)

    fun update (a:dom, r:'_a, (_,h): '_a qmap) = H.insert h (a,r)           (************************)
                                                                            (* Always do the update *)
									    (* in the hash table.   *)
                                                                            (************************)
	(* I guess its an error to make updates to entries that appear
	 * in the persistent map -  mael 2004-04-07. We could insert a
	 * check here to see if this ever happens... *)
	

    fun Fold (f : ((dom * 'b) * 'a) -> 'a) (acc : 'a) ((m,h):'b qmap) =
      let fun not_in_hash_table (a,r) = 					(*******************************)
	    case H.peek h a of SOME _ => false | NONE => true	  	        (* First, filter out things in *)
	  val m' = Env.filter not_in_hash_table m				(* map that are in hash table  *)
	  val acc' = Env.Fold f acc m' 	                                        (*******************************)
      in List.foldl f acc' (H.listItems h)  (* was acc ; mael 2005-02-08 *)
      end

    fun combine(m_pure, m_imp as (discard, h)) = (m_pure, h)


    type StringTree = PP.StringTree                                       (*******************)
    fun layout {start:string,finish:string,eq:string,sep:string} 	  (* Pretty Printing *)
      (layout_dom: dom -> StringTree) (layout_ran: 'a -> StringTree) 	  (*******************)
      (q : 'a qmap) : StringTree =
      let fun layout_entry(a,r) = PP.NODE{start="",finish="",childsep=PP.RIGHT eq, indent=0,
					  children=[layout_dom a, layout_ran r]}
      in PP.NODE{start=start,finish=finish,childsep=PP.RIGHT sep, indent=1,
		 children=Fold (fn (entry,l) => layout_entry entry :: l) [] q}
      end

  end
