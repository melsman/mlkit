structure IntSet = Set(type elem=int
		       val lt = op <
		       val pr = toString)

val set1 = 
  let open IntSet
      val U = IntSet.union
      val S = singleton
      infixr U
  in (S 3) U (S 4) U (S 8) U (S 10) U (S 3)
  end

val _ = writeln (IntSet.pr set1)
