(* Opaque matching done in ArraySlice.sml *)
structure VectorSlice : VECTOR_SLICE =
    TableSlice(type 'a table = 'a vector
	       val maxLen = Vector.maxLen)
