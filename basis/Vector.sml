structure Vector : VECTOR =
    let structure T = PolyTable (type 'a table = 'a vector)
    in struct
	   open T
	   val update = updatev
       end
    end

fun vector l = Vector.fromList l
