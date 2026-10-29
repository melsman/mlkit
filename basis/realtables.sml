(* Packed real tables use the same collection functors as integer tables.
 * Bounds and slice operations remain specialized at each application. *)
functor TableArgReal32(type table) : WORD_TABLE_ARG =
struct
  type elem = Real32.real
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

local
  structure T = BuildTables(structure ArgV = TableArgReal32(type table = string)
                            structure ArgA = TableArgReal32(type table = chararray))
in
  (** SigDoc *)
  structure Real32Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Real32VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Real32Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Real32ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Real32Array2 : MONO_ARRAY2 = T.A2
end
