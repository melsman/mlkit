  val f = let val x = ref (2, [1])
	  in fn y => (#1 (!x), y)
	  end 
  val r = f 5
