       fun g() = 
         let val x = [5,7]
             fun f(y) = (if y>3 then x@x else x;
                         5)
         in 
             f 1; f 4
         end;

