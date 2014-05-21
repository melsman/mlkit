structure ListUtils =
struct


  (* [member x l] from the Edinburgh Library
         Created by:	Dave Berry, LFCS, University of Edinburgh
                        db@lfcs.ed.ac.uk
         Date:	        4 Oct 1989
         Maintenance:	Author 
  *)
  fun member _ [] = false
  |   member x (h::t) =
	x = h orelse member x t

  (* [stringSep start finish sep p l] from the Edinburgh Library
         Created by:	Dave Berry, LFCS, University of Edinburgh
                        db@lfcs.ed.ac.uk
         Date:	        4 Oct 1989
         Maintenance:	Author 
  *)
  local
    fun str _ nil _ _ = ""
    |   str p (h::t) sep needSep =
          let val s = p h ^ (str p t sep true)
           in if needSep then sep ^ s else s
          end
  in
    fun stringSep start finish sep p l = start ^ (str p l sep false) ^ finish
  end

end (* structure ListUtils *)