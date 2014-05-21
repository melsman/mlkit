   fun g (n,b) = h (n-1) b
   and h 0 b = b
     | h n b = g(n,n*b)
