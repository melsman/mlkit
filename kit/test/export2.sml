  fun app (n:int) (acc:int) = if n <= 0 then acc else
      let val g = if n div 2 = 0 then fn x => x+2
		  else fn x => x+1
	  val () = _export("appML", g)
          val acc :int = prim("appML", acc)
      in app (n-1) acc
      end

  val _ = print ("Result: " 
                 ^ Int.toString(app 2 0) 
                 ^ "\n")
