fun g() = 
  let fun mk_x() = [5,7]
      fun f(y) = let val x = mk_x()
                 in if y>3 then x@x else x; 5 
                 end
  in 
      f 1; f 4
  end;
