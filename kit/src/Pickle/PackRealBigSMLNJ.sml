structure PackRealBig : PACK_REAL =
    struct
	type real = real
	val bytesPerElem = 8
	val isBigEndian = true
	fun toBytes (r:real) : Word8Vector.vector =
	    let	
		fun toBytesL(r:real): Word8.word list =
		    let val ra = RealArray.array(1, r)
			val ba : Word8Array.array = Unsafe.cast ra
			fun b i = Unsafe.Word8Array.sub(ba, i)
		    in
			[b 0, b 1, b 2, b 3, b 4, b 5, b 6, b 7]
		    end
	    in Word8Vector.fromList(toBytesL r)
	    end

	fun fromBytes (bv: Word8Vector.vector) : real =
	    let val ra : RealArray.array = Unsafe.cast bv
	    in RealArray.sub(ra, 0)
	    end
    end
