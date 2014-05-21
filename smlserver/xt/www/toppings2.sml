functor toppings2 (F : sig val toppings : string list Form.var
		       end) : SCRIPTLET =
    struct
	open Scripts infix &	    

	val response = 
	    case Form.get F.toppings of
		Form.Ok nil => 
		    Page.page "Pizza Order" (p($"You ordered a pizza with no toppings."))
	      | Form.Ok (all as (t :: ts)) => 
		    Page.page "Pizza Order" 
		    (p($ ("You ordered a pizza with the following " 
			  ^ Int.toString (length all) 
			  ^ " toppings:")) &
		     ul (flatten (li($t), map (li o $) ts)))
	      | _ => 
		    Page.page "Error" (p($"impossible - unless you are tampering the request"))

    end
