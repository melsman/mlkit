functor sum (F : sig val sum : int Obj.obj
		     val n : int Obj.obj
		 end) : SCRIPTLET =
    struct
	open Scripts infix && ++

	val response = 
	    case (Obj.valOf F.sum, Obj.valOf F.n) of
		(SOME sum, SOME n) =>
		    if n <= 0 then
			Page.page "Sum" 
			(p ($ ("Sum is " ^ Int.toString sum)))
		    else sum.redirect {sum=sum+n,n=n-1} nil
	      | _ => Page.page "Error" (p($"Wrong sum, count, or both"))
    end
