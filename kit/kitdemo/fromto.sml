       fun fromto(a, b) = if a>b then []
                          else a :: fromto(a+1, b)
       val l = #1(fromto(1,10), fromto(100,110))

