functor F (A : sig type a end) = 
  struct
     type u = A.a
     type s = A.a
  end :> 
  sig
     type s
     type u = A.a
  end

functor G () = F (datatype a = S)

structure R = G ()
