   fun upto n = 
     let fun loop(n,acc) = if n=0 then acc
			   else loop(n-1, n::acc)
     in loop(n,[])
     end

   fun nlength [] = 0
     | nlength (_::xs) = 1 + nlength xs

   fun tlength(l) =
     let fun tlength'(nil, acc) = acc
	   | tlength'(_::xs, acc) = tlength'(xs,acc+1)
     in tlength'(l,0)
     end

   fun klength l =
     let fun loop(p as ([], acc)) = p
	   | loop(_::xs, acc) = loop(xs,acc+1)
     in #2(loop(l,0))
     end

   local 
     fun llength'(p as ([], acc)) = p
       | llength'(_::xs, acc) = llength'(xs,acc+1)
   in
     fun llength(l) = #2(llength'(l, 0))
   end

   fun global(p as ([], acc)) = p
     | global(_::xs, acc) = global(xs, acc+1)

   fun glength(l) = #2(global(l, 0))

   val k = 10000
   val run = 
     nlength(upto k) + tlength(upto k) + klength(upto k) 
     + llength(upto k) + glength(upto k);
