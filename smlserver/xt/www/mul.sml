functor mul (F : sig val sz : int Form.var 
		 end) : SCRIPTLET =
    struct
	open Scripts infix & % attr

        fun iter f n = if n <= 1 then flatten(f 1,nil)
		       else iter f (n-1) & f n
	fun col r c = 
	    (td (* attr (A.align A.center) *))
	    ($(Int.toString ( r * c )))
	    
	fun row sz r = 
	    tr (iter (col r) sz)

	fun tab sz = 
	    (table attr (A.border 4 % A.width (A.pct 90) 
			 % A.frame A.vsides % A.rules A.rows))
	    (iter (row sz) sz)

	val sz = Page.get "Size" F.sz

	val response = 
	    Page.page "Multiplication table" (tab sz)
    end
