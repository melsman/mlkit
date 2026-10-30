(* Table applications; keep slice and bounds helpers specialized here. *)

local
  structure T =
    BuildTables(structure ArgV = TableArgWord63(type table = string)
                structure ArgA = TableArgWord63(type table = chararray))
in
  (** SigDoc *)
  structure Word63Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Word63VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Word63Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Word63ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Word63Array2 : MONO_ARRAY2 = T.A2
end

local
  structure T =
    BuildTables(structure ArgV = TableArgWord64(type table = string)
                structure ArgA = TableArgWord64(type table = chararray))
in
  (** SigDoc *)
  structure Word64Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Word64VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Word64Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Word64ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Word64Array2 : MONO_ARRAY2 = T.A2
  (** SigDoc *)
  structure LargeWordVector : MONO_VECTOR = Word64Vector
  (** SigDoc *)
  structure LargeWordVectorSlice : MONO_VECTOR_SLICE = Word64VectorSlice
  (** SigDoc *)
  structure LargeWordArray : MONO_ARRAY = Word64Array
  (** SigDoc *)
  structure LargeWordArraySlice : MONO_ARRAY_SLICE = Word64ArraySlice
  (** SigDoc *)
  structure LargeWordArray2 : MONO_ARRAY2 = Word64Array2
end

local
  structure T =
    BuildTables(structure ArgV = TableArgWord(type table = string)
                structure ArgA = TableArgWord(type table = chararray))
in
  (** SigDoc *)
  structure WordVector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure WordVectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure WordArray : MONO_ARRAY = T.A
  (** SigDoc *)
  structure WordArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure WordArray2 : MONO_ARRAY2 = T.A2
end
