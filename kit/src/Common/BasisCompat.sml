structure BasisCompat =
struct

    structure ListPair = struct
        exception UnequalLengths
        fun zipEq (xs, ys) = 
          let fun h (x::xr) (y::yr) res = h xr yr ((x, y) :: res)
		| h []      []      res = List.rev res
                | h _       _       res = raise UnequalLengths
	  in h xs ys [] 
	  end
    end (* structure ListPair *)

end (* structure BasisCompat *)