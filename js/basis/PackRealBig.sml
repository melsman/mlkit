structure PackRealBig : PACK_REAL =
  struct
    type real = real
    val bytesPerElem = 8
    val isBigEndian = true
    fun reverse_gen length sub vec =
        let val len = length vec
        in  Word8Vector.tabulate(len, fn i => sub(vec, len - 1 - i))
        end

    fun reverse vec = reverse_gen Word8Vector.length Word8Vector.sub vec

    structure Little = PackRealLittle

    fun toBytes r = reverse(Little.toBytes r)
    fun fromBytes vec = Little.fromBytes(reverse vec)
    fun subVec (vec, i) =
        Little.fromBytes(
            reverse_gen Word8VectorSlice.length Word8VectorSlice.sub (
                Word8VectorSlice.slice(vec, i*bytesPerElem, SOME bytesPerElem)))

    fun subArr (vec, i) =
        Little.fromBytes(
            reverse_gen Word8ArraySlice.length Word8ArraySlice.sub (
                Word8ArraySlice.slice(vec, i*bytesPerElem, SOME bytesPerElem)))

    fun update (arr, i, r) =
        Word8Array.copyVec {src=toBytes r, dst=arr, di=i*bytesPerElem}

  end
