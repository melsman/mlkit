       local
         fun cp [] =[]
           | cp (x::xs)= x :: cp xs

       (* exormorphic merge *)
         fun merge(xs, []):int list = cp xs
           | merge([], ys) = cp ys
           | merge(l1 as x::xs, l2 as y::ys) = 
               if x<y then x :: merge(xs, l2) 
               else y :: merge(l1, ys)


         (* splitting a list *)
         fun split(x::y::zs, l, r) = split(zs, x::l, y::r)
           | split(x::xs, l, r) = (xs, x::l, r)
           | split(p as ([], l, r)) = p

  
         infix footnote
         fun x footnote y = x

         (* exomorphic merge sort *)
         fun msort []  = []
           | msort [x] = [x]
           | msort xs = let val (_, l, r) = split(xs, [], [])
                        in resetRegions xs;
                           merge(msort l footnote resetRegions l, 
                                 msort r footnote resetRegions r)
                        end

       in
         val runmsort = msort(upto(50000))
 
         val result = output(std_out, "Really done\n");
       end
