functor F(structure S: sig type 'a t end) =
   struct
      datatype t = T of u S.t
      withtype u = int
   end
