fun g() = 
  let 
      fun f(x,y) = (if y>3 then x@x else x; 5)

  in 
      let val x = [5,7]
      in  f(x, 1); f(x, 4)
      end
  end;
