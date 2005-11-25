structure ArraySlice :> ARRAY_SLICE =
    TableSlice(type 'a table = 'a array
	       val maxLen = Array.maxLen)
    
structure VectorSlice :> VECTOR_SLICE = VectorSlice