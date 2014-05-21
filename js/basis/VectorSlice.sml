(* Opaque matching done in ArraySlice.sml *)
structure VectorSlice :> VECTOR_SLICE =
    TableSlice(type 'a table = 'a vector
	       val maxLen = Vector.maxLen
	       type 'a vector_slice = 'a vector * int * int
	       val vector_slice_base = fn x => x)
