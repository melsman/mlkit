structure S =
   struct
      datatype 'a t = T of 'a u
      and 'a u = U
   end

functor F() = S

structure K = F()

val a = K.U = K.U