fun upto n = 
  let fun loop(p as (0,acc)) = p
        | loop(n, acc) = 
            loop(n-1, n::acc)
  in
      #2(loop(n,[]))
  end

       