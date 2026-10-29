(* ---------------------------------------------------------------
   Here comes the basic functors for building the argument structures
   to the BuildTables functor. There is one functor for each
   underlying type (e.g., word31, bool, real, ...) The functors are
   applied in the file wordtables.sml.
   --------------------------------------------------------------- *)

functor TableArgBool(type table) : WORD_TABLE_ARG = struct
  type elem = bool
  fun w2b (w:Word8.word) : bool = (w = 0w1)
  fun b2w (b:bool) : Word8.word = if b then 0w1 else 0w0
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  fun vsub (t:vector,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  type table = table
  fun tsub (t:table,i:int) : elem = w2b(prim ("__bytetable_sub", (t,i)))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,b2w e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgWord8(type table) : WORD_TABLE_ARG = struct
  type elem = Word8.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgWord16(type table) : WORD_TABLE_ARG = struct
  type elem = Word16.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 2
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w1))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord31(type table) : WORD_TABLE_ARG = struct
  type elem = Word31.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w4))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord32(type table) : WORD_TABLE_ARG = struct
  type elem = Word32.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w4))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord63(type table) : WORD_TABLE_ARG = struct
  type elem = Word63.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord64(type table) = struct
  type elem = Word64.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgWord(type table) : WORD_TABLE_ARG = struct
  type elem = Word.word
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgReal(type table) : WORD_TABLE_ARG = struct
  type elem = real
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun asub (t:array,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__blockf64_sub_real", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__blockf64_update_real", (t,i,e))
  fun tlen (t:table) : int = prim ("__blockf64_size", t)
  fun vlen (t:vector) : int = prim ("__blockf64_size", t)
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt8(type table) : WORD_TABLE_ARG = struct
  type elem = Int8.int
  type array = chararray
  type vector = string
  val wordSizeBytes = 1
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update", (t,i,e))
  fun tlen (t:table) : int = prim ("__bytetable_size", t)
  fun vlen (t:vector) : int = prim ("__bytetable_size", t)
  fun talloc (n:int) : table = prim("allocStringML", n)
  fun valloc (n:int) : vector = prim("allocStringML", n)
  fun aalloc (n:int) : array = prim("allocStringML", n)
end

functor TableArgInt16(type table) : WORD_TABLE_ARG = struct
  type elem = Int16.int
  type array = chararray
  type vector = string
  val wordSizeBytes = 2
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w2))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w1))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word16", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt31(type table) : WORD_TABLE_ARG = struct
  type elem = int31
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w4))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word31", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word31", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt32(type table) : WORD_TABLE_ARG = struct
  type elem = int32
  type array = chararray
  type vector = string
  val wordSizeBytes = 4
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w4))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word32", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word32", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt63(type table) : WORD_TABLE_ARG = struct
  type elem = int63
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word63", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word63", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt64(type table) = struct
  type elem = int64
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word64", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word64", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end

functor TableArgInt(type table) : WORD_TABLE_ARG = struct
  type elem = int
  type array = chararray
  type vector = string
  val wordSizeBytes = 8
  fun toBytes (n:int) : int = Word.toIntX(Word.*(Word.fromInt n, 0w8))
  fun fromBytes (n:int) : int = Word.toIntX(Word.>>(Word.fromInt n, 0w3))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun aupd (t:array,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun vupd (t:vector,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  type table = table
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word", (t,i))
  fun tupd (t:table,i:int,e:elem) : unit = prim("__bytetable_update_word", (t,i,e))
  fun tlen (t:table) : int = fromBytes(prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes(prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim("allocStringML", toBytes n)
end
