(*$QuasiEnv: QUASI_ENV CRASH ORDER_FINMAP PRETTYPRINT HASH_TABLE*)

(* Environment with a persistent part to be used accross program units
 * ('a map) and a combined persistent/non-persistent (hashing) map to
 * be used locally for each program unit ('a qmap).  
 *)

functor QuasiEnv(structure OFinMap : ORDER_FINMAP
		 structure HashTable : HASH_TABLE
		 val key : OFinMap.dom -> int
		 structure PP : PRETTYPRINT
		   sharing type PP.StringTree = OFinMap.StringTree = HashTable.StringTree
		 structure Crash : CRASH) : QUASI_ENV =
  struct
    structure H = HashTable
    structure Env = OFinMap

    fun die s = Crash.impossible ("QuasiEnv." ^ s)

    type '_a map = '_a Env.map
    type '_a qmap = '_a map * '_a H.hash_table
    type dom = Env.dom

    fun mk (n : int) (m : '_a map) : '_a qmap = (m, H.mk_empty n)

    fun lookup ((m,h) : '_a qmap) (a : dom) : '_a option =           (************************)
      case H.lookup(h,key a)                                         (* First, lookup in the *)
	of (r as SOME _) => r                                        (* hash table.          *)
	 | NONE => Env.lookup m a                                    (************************)

    fun update (a:dom, r:'_a, (_,h): '_a qmap) = H.update(h,key a,r)        (************************)
                                                                            (* Always do the update *)
									    (* in the hash table.   *)
                                                                            (************************)

    fun Fold (f : ((int * '_b) * 'a) -> 'a) (acc : 'a) ((m,h):'_b qmap) =
      let fun not_in_hash_table (a,r) = 					(*******************************)
	    case H.lookup(h,key a) of SOME _ => false | NONE => true		(* First, filter out things in *)
	  val m' = Env.filter not_in_hash_table m				(* map that are in hash table  *)
	  fun f' ((dom,b),a) = f ((key dom,b),a)				(*******************************)	    
	  val acc' = Env.Fold f' acc m' 	
      in H.Fold f acc' h
      end

    fun combine(m_pure, m_imp as (discard, h)) = (m_pure, h)


    type StringTree = PP.StringTree                                       (*******************)
    fun layout {start:string,finish:string,eq:string,sep:string} 	  (* Pretty Printing *)
      (layout_dom: int -> StringTree) (layout_ran: '_a -> StringTree) 	  (*******************)
      (q : '_a qmap) : StringTree =
      let fun layout_entry(a,r) = PP.NODE{start="",finish="",childsep=PP.RIGHT eq, indent=0,
					  children=[layout_dom a, layout_ran r]}
      in PP.NODE{start=start,finish=finish,childsep=PP.RIGHT sep, indent=1,
		 children=Fold (fn (entry,l) => layout_entry entry :: l) [] q}
      end

  end
