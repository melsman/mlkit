structure Vector : VECTOR = 
  WordTable (type 'a table = 'a vector)

fun vector l = Vector.fromList l



