let fun cast (a: 'a) : 'b = 
      let datatype t = C of 'c
      in case C a
           of C b => b
      end
in cast true + 5
end;
