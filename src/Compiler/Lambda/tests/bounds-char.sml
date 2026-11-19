(* All character indexes fit. Compile this reusable unit to check that bounds
 * checks disappear even when an imported access was already lowered to an
 * unsigned comparison before the character range became known. *)
val table = Array.array (256,0)
val vector = Vector.tabulate (256,fn i => i)
fun readChar c = Array.sub (table,Char.ord c)
fun readVectorChar c = Vector.sub (vector,Char.ord c)
fun writeChar (c,x) = Array.update (table,Char.ord c,x)
val literalIndex = Array.sub (table,66)

(* An exported explicit check exercises lowering in this unit, independently
 * of whether the imported Basis has already lowered its own checks. *)
fun within256 i = 0 <= i andalso i < 256

(* Combining the subscript check must not hide it from loop elimination. *)
fun byteSum s =
  let val n = String.size s
      fun loop (i,sum) =
        if i < n then loop(i+1,sum+Char.ord(String.sub(s,i))) else sum
  in loop(0,0)
  end
