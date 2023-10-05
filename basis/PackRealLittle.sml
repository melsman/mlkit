structure PackRealLittle : PACK_REAL =
  struct
    type real = real
    val bytesPerElem = 8
    val isBigEndian = false
    fun toBytesS (r: real) : string = prim("sml_real_to_bytes",r)
    fun fromBytesS (s:string) : real = prim("sml_bytes_to_real",s)

    fun toBytes (r:real) : Word8Vector.vector =
        Byte.stringToBytes(toBytesS r)

    fun fromBytes (s: Word8Vector.vector) : real =
        fromBytesS(Byte.bytesToString s)

    fun subVec (v,i) =
        let
          fun toL 9 l = l
            | toL j l = toL (j+1) (Word8Vector.sub(v,(i+1)*bytesPerElem - j) :: l)
        in fromBytes (Word8Vector.fromList (toL 1 []))
        end

    fun subArr (a,i) =
        let
          fun toL 9 l = l
            | toL j l = toL (j+1) (Word8Array.sub(a,(i+1)*bytesPerElem-j) :: l)
        in fromBytes (Word8Vector.fromList (toL 1 []))
        end

    fun update (a,i,r) =
        Word8Array.copyVec {src=toBytes r, dst=a, di=i*bytesPerElem}
  end

(** SigDoc *)
structure PackReal64Little : PACK_REAL = PackRealLittle
