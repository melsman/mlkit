(* Binary64 mono collections share the packed collection functors. *)
functor TableArgReal(type table) : WORD_TABLE_ARG =
struct
  type elem = real
  type array = chararray
  type vector = string
  type table = table
  val wordSizeBytes = 8
  fun toBytes n = Word.toIntX (Word.* (Word.fromInt n,0w8))
  fun fromBytes n = Word.toIntX (Word.>> (Word.fromInt n,0w3))
  fun asub (t:array,i:int) : elem =
      prim ("execStmtJS", ("return SmlPrims.real64Sub(t,i);","t,i",t,i))
  fun aupd (t:array,i:int,x:elem) : unit =
      prim ("execStmtJS", ("return SmlPrims.real64Update(t,i,x);","t,i,x",t,i,x))
  fun vsub (t:vector,i:int) : elem =
      prim ("execStmtJS", ("return SmlPrims.real64Sub(t,i);","t,i",t,i))
  fun vupd (t:vector,i:int,x:elem) : unit =
      prim ("execStmtJS", ("return SmlPrims.real64Update(t,i,x);","t,i,x",t,i,x))
  fun tsub (t:table,i:int) : elem =
      prim ("execStmtJS", ("return SmlPrims.real64Sub(t,i);","t,i",t,i))
  fun tupd (t:table,i:int,x:elem) : unit =
      prim ("execStmtJS", ("return SmlPrims.real64Update(t,i,x);","t,i,x",t,i,x))
  fun tlen (t:table) : int = fromBytes (prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes (prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim ("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim ("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim ("allocStringML", toBytes n)
end
functor TableArgReal32(type table) : WORD_TABLE_ARG =
struct
  type elem = real32
  type array = chararray
  type vector = string
  type table = table
  val wordSizeBytes = 4
  fun toBytes n = Word.toIntX (Word.* (Word.fromInt n,0w4))
  fun fromBytes n = Word.toIntX (Word.>> (Word.fromInt n,0w2))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_real32", (t,i))
  fun aupd (t:array,i:int,x:elem) : unit = prim ("__bytetable_update_real32", (t,i,x))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_real32", (t,i))
  fun vupd (t:vector,i:int,x:elem) : unit = prim ("__bytetable_update_real32", (t,i,x))
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_real32", (t,i))
  fun tupd (t:table,i:int,x:elem) : unit = prim ("__bytetable_update_real32", (t,i,x))
  fun tlen (t:table) : int = fromBytes (prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes (prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim ("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim ("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim ("allocStringML", toBytes n)
end

functor TableArgReal16(type table) : WORD_TABLE_ARG =
struct
  type elem = real16
  type array = chararray
  type vector = string
  type table = table
  val wordSizeBytes = 2
  fun toBytes n = Word.toIntX (Word.* (Word.fromInt n,0w2))
  fun fromBytes n = Word.toIntX (Word.>> (Word.fromInt n,0w1))
  fun asub (t:array,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun aupd (t:array,i:int,x:elem) : unit = prim ("__bytetable_update_word16", (t,i,x))
  fun vsub (t:vector,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun vupd (t:vector,i:int,x:elem) : unit = prim ("__bytetable_update_word16", (t,i,x))
  fun tsub (t:table,i:int) : elem = prim ("__bytetable_sub_word16", (t,i))
  fun tupd (t:table,i:int,x:elem) : unit = prim ("__bytetable_update_word16", (t,i,x))
  fun tlen (t:table) : int = fromBytes (prim ("__bytetable_size", t))
  fun vlen (t:vector) : int = fromBytes (prim ("__bytetable_size", t))
  fun talloc (n:int) : table = prim ("allocStringML", toBytes n)
  fun valloc (n:int) : vector = prim ("allocStringML", toBytes n)
  fun aalloc (n:int) : array = prim ("allocStringML", toBytes n)
end
