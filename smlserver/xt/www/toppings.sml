functor toppings () : SCRIPTLET =
    struct
	open Scripts infix &&

	val response = 
	    Page.page "Pizza Order Form" 
	    (p (toppings2.link {toppings=["cheese", "pepperoni"]} ($"Cheese-Pepperoni Quick Order"))
	     &&
	     toppings2.form 
	     (p( $ "What toppings would you like on your pizza?")
	      && checkboxDrop 
	      (table (tr (td ($"Cheese") && 
			  td (inputCheckbox' toppings2.toppings (Obj.fromString "cheese")))  && 
		      tr (td ($"Pepperoni") && 
			  td(inputCheckbox' toppings2.toppings (Obj.fromString "pepperoni"))) &&
		      tr (td ($"Ananas") && 
			  td(inputCheckbox toppings2.toppings (Obj.fromString "ananas"))))
	       && p(inputSubmit "Order pizza"))))
    end
