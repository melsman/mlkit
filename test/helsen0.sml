val b = true
val test = let val g1 = fn y => y
               val g2 = fn z => z
           in
             fn y => (let val f1 = fn x => ((fn z => 3)
                                            (if b then (g1 7)
                                                  else (g2 8) ))
                         val f2 = fn f => 4
                     in
                        f2 f1
                     end)
           end 2