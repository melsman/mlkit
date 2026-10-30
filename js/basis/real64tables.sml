(* Binary64 mono collections share the packed collection functors. *)
functor TableArgReal(type table) : WORD_TABLE_ARG =
struct
  type elem = Real.real
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
local
  structure T = BuildTables(structure ArgV = TableArgReal(type table = string)
                            structure ArgA = TableArgReal(type table = chararray))
in
  (** SigDoc *)
  structure RealVector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure RealVectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure RealArray : MONO_ARRAY = T.A
  (** SigDoc *)
  structure RealArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure RealArray2 : MONO_ARRAY2 = T.A2
  (** SigDoc *)
  structure Real64Vector : MONO_VECTOR = RealVector
  (** SigDoc *)
  structure Real64VectorSlice : MONO_VECTOR_SLICE = RealVectorSlice
  (** SigDoc *)
  structure Real64Array : MONO_ARRAY = RealArray
  (** SigDoc *)
  structure Real64ArraySlice : MONO_ARRAY_SLICE = RealArraySlice
  (** SigDoc *)
  structure Real64Array2 : MONO_ARRAY2 = RealArray2
  (** SigDoc *)
  structure LargeRealVector : MONO_VECTOR = RealVector
  (** SigDoc *)
  structure LargeRealVectorSlice : MONO_VECTOR_SLICE = RealVectorSlice
  (** SigDoc *)
  structure LargeRealArray : MONO_ARRAY = RealArray
  (** SigDoc *)
  structure LargeRealArraySlice : MONO_ARRAY_SLICE = RealArraySlice
  (** SigDoc *)
  structure LargeRealArray2 : MONO_ARRAY2 = RealArray2
end
