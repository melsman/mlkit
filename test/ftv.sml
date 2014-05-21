fun g h v = h v

functor F() : sig end = 
    struct
	val z = g (fn _ => ())
    end

structure S1 = F()

structure S2 = F();