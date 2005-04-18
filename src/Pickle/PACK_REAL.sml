(*
 * Copyright, Martin Elsman 2003-01-07 
 * GPL Licence
 *)

signature PACK_REAL =
    sig
	type real
	val bytesPerElem : int
	val isBigEndian : bool
	val toBytes : real -> Word8Vector.vector
	val fromBytes : Word8Vector.vector -> real
    end
