functor mul (F : sig val sz : int Obj.obj end) : SCRIPTLET =
    struct
	open Scripts infix && ++

        fun iter f n = if n <= 1 then f 1
		       else iter f (n-1) ++ f n
	fun col r c = 
	    tda (A.align A.center)
	    ($(Int.toString ( r * c )))
	    
	fun row sz r = 
	    tr (iter (col r) sz)

	fun tab sz = 
	    tablea (A.border 1) (iter (row sz) sz)

	val response = 
	    case Obj.valOf F.sz of
		SOME sz => 
		    Page.page "Multiplication table" (tab sz)
	      | NONE => 
		    Page.page "Error" (p($"Expecting an integer"))
    end
