functor bmi2 (F : sig val h : int Obj.obj
		      val w : int Obj.obj
		  end) : SCRIPTLET =
    struct
	open Scripts infix &&
	
	val response = 
	    case (Obj.valOf F.h, Obj.valOf F.w) of
		(SOME h, SOME w) =>
		    let val bmi = Int.div(w * 10000, h * h)
			val txt = if bmi > 25 then "too high!"
				  else if bmi < 20 then "too low!"
				       else "normal"
		    in Page.page "Body Mass Index" 
			(h2 ($"Your BMI is " && $txt))
		    end
	      | _ => Page.page "Body Mass Index" (p($"Error"))
    end
