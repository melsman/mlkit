(*Byte.sml*)

structure Byte : BYTE = struct
  fun byteToChar (x : Word8.word) : Char.char  = prim ("id", x)
  fun charToByte (x : Char.char)  : Word8.word = prim ("id", x)
  fun bytesToString (x : Word8Vector.vector) : String.string = x
  fun stringToBytes (x : String.string) : Word8Vector.vector = x
  fun unpackStringVec arg = bytesToString (Word8Vector.extract arg)
  fun unpackString arg = bytesToString (Word8Array.extract arg)

  fun packString (ss, a, i) =
        let val (s, si, n) = Substring.base ss
	in  
	  Word8Array.copyVec {src = stringToBytes s, si = si, 
			      len = SOME n, dst = a, di = i} 
	end
end; (*structure Byte*)
