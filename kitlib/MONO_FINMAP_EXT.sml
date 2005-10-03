
(* Modified version of the MONO_SET signature given in the Edinburgh
   library. *)

signature MONO_FINMAP_PP =
sig
    include MONO_FINMAP

    val layoutMap : {start: string, eq: string, sep: string, finish: string} 
	-> (dom -> PrettyPrint.StringTree) 
	-> ('b -> PrettyPrint.StringTree) 
	-> 'b map -> PrettyPrint.StringTree

    val reportMap : (dom * 'b -> Report.Report) 
	-> 'b map -> Report.Report
end

signature MONO_FINMAP_EXT =
sig
    include MONO_FINMAP_PP    		       
    val pu : 'a Pickle.pu -> 'a map Pickle.pu
end

functor MonoFinMapPP (FM : MONO_FINMAP) : MONO_FINMAP_PP =
struct 
  open FM
  local structure PP = PrettyPrint
  in
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

      fun reportMap f t = Report.flatten(map f (list t))  
  end
end
