functor index () : SCRIPTLET =
    struct
	open Scripts infix && ++

	val response = Page.page "Examples"
	    (ul 
	     (
	      li (time_of_day.link ($"Time of day"))
	      ++ li (mul.link {sz=12} ($"Multiplication table"))
	      ++ li (temp.link ($"Temperature conversion"))
	      ++ li (count.link {c=0} ($"Counter"))
	      )
	     )	    
    end
