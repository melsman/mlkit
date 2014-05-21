functor sum (F : sig val sum : int Form.var
		     val n : int Form.var
		 end) : SCRIPTLET =
    struct
	open Scripts infix & %

	val response = 
	    case (Form.get F.sum, Form.get F.n) of
		(Form.Ok sum, Form.Ok n) =>
		    if n <= 0 then
			Page.page "Sum" 
			(p ($ ("Sum is " ^ Int.toString sum)))
		    else sum.redirect {sum=sum+n,n=n-1} nil
	      | _ => Page.page "Error" (p($"Wrong sum, count, or both"))
    end
