signature A =
  sig
      type t
      val C : t
  end

functor F() =
  struct
      type t = int
      val C : t = 4
  end :> A 

structure S0 = F() :> A
