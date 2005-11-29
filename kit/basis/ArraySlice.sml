structure ArraySlice :> ARRAY_SLICE =
    TableSlice(type 'a table = 'a array
	       val maxLen = Array.maxLen
	       type 'a vector_slice = 'a VectorSlice.slice
	       val vector_slice_base = VectorSlice.base)
