structure PackRealLittle : PACK_REAL =
    struct
	type real = real
	val bytesPerElem = 8
	val isBigEndian = false
	fun toBytes (r:real) : Word8Vector.vector =
	    let val ra = RealArray.array(1, r)
		val ba : Word8Array.array = Unsafe.cast ra
		fun b i = Unsafe.Word8Array.sub(ba, i)
	    in
		Word8Vector.tabulate(8,b)
	    end
	fun fromBytes (bv: Word8Vector.vector) : real =
	    let val ra : RealArray.array = Unsafe.cast bv
	    in RealArray.sub(ra, 0)
	    end
    end
