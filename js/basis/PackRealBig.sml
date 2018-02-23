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

    fun subVec (v,i) = PackRealLittle.subVec(vrev v, i)
    fun subArr (a,i) = subVec(Word8Vector.tabulate
                                 (bytesPerElem, fn j => Word8Array.sub(a,j+i*bytesPerElem)),
                              bytesPerElem)
    fun update (a,i,r) = Word8Array.copyVec {src=toBytes r, dst = a, di = i}
  end
