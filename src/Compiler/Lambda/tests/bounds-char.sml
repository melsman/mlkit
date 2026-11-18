(* All character indexes fit. Compile this reusable unit to check that bounds
 * checks disappear even when an imported access was already lowered to an
 * unsigned comparison before the character range became known. *)
val table = Array.array (256,0)
val vector = Vector.tabulate (256,fn i => i)
fun readChar c = Array.sub (table,Char.ord c)
fun readVectorChar c = Vector.sub (vector,Char.ord c)
fun writeChar (c,x) = Array.update (table,Char.ord c,x)
val literalIndex = Array.sub (table,66)
