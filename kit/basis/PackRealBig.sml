structure PackRealBig : PACK_REAL =
    struct
	type real = real
	val bytesPerElem = 8
	val isBigEndian = true
	fun toBytesS (r: real) : string = prim("sml_real_to_bytes",r)
	fun fromBytesS (s:string) : real = prim("sml_bytes_to_real",s)

	fun toBytes (r:real) : Word8Vector.vector =
	    Byte.stringToBytes(toBytesS r)
	    
	fun fromBytes (s: Word8Vector.vector) : real =
	    fromBytesS(Byte.bytesToString s)
    end
