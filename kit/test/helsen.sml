val test = let val g = fn y (* : int *) => y in
               fn y => (let val f1 = fn x => (g 7)
                            val f2 = fn f => 4
                        in
                           f2 f1
                        end)
           end 2