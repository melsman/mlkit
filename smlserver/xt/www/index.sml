functor index () : SCRIPTLET =
    struct
	open Scripts infix &

	val response = Page.page "Examples"
	    (ul 
	     (  li (time_of_day.link ($"Time of day"))
	      & li (mul.link {sz=12} ($"Multiplication table"))
	      & li (temp.link ($"Temperature conversion"))
	      & li (count.link {c=0} ($"Counter"))
	      & li (sum.link {n=5,sum=0} ($"HTTP Sum"))
	      & li (questionnaire.link ($"Sample questionnaire"))
	      & li (toppings.link ($"Pizza toppings"))
	      & li (bmi.link ($"Body Mass Index"))
	      & li (countreload.link ($"Count Reloads"))
	      )
	     )	    
    end
