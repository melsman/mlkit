structure PackRealBig : PACK_REAL =
    struct
	type real = real
	val bytesPerElem = 8
	val isBigEndian = true
	fun vrev v = 
	    let val len = Word8Vector.length v
	    in Word8Vector.tabulate(len,fn i => Word8Vector.sub(v,len-1-i))
	    end
	fun toBytes (r:real) : Word8Vector.vector =
	    vrev(PackRealLittle.toBytes r)	    
	fun fromBytes (v: Word8Vector.vector) : real =	    
	    PackRealLittle.fromBytes(vrev v)
    end
