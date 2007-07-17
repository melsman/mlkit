(* Byte structure - partly from Moscow ML; mael 2005-11-27 *)
structure Byte : BYTE = 
  struct
      fun byteToChar (x : Word8.word) : Char.char  = prim ("id", x)
      fun charToByte (x : Char.char)  : Word8.word = prim ("id", x)
      fun bytesToString (x : Word8Vector.vector) : String.string = x
      fun stringToBytes (x : String.string) : Word8Vector.vector = x
      fun unpackStringVec (a,b,c) = 
	  bytesToString (Word8VectorSlice.vector (Word8VectorSlice.slice (a,b,SOME c)))
      fun unpackString (a,b,c) = 
	  bytesToString (Word8ArraySlice.vector (Word8ArraySlice.slice (a,b,SOME c)))
	  
      fun packString (a, i, ss) =
	  let val (s, si, n) = Substring.base ss
	      val src = Word8VectorSlice.slice(stringToBytes s, si, SOME n)
	  in  
	      Word8ArraySlice.copyVec {src = src, dst = a, di = i} 
	  end
  end
