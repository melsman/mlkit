structure S = 
  struct
    fun f a = a
  end

functor A() = S

functor B ( val f : string -> string )  = 
  struct val f = f 
  end

functor C() = B( A() )

structure M = C()

val _ = print (M.f "Ok\n")