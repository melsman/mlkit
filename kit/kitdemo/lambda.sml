val n = let val f = let val xs = [1,2]
                    in fn ys => length xs + length ys
                    end
        in f [7]
        end
             