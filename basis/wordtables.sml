(* Applications of the WordTable and WordSlice functors (defined in
   wordtable-functors.sml) for various word-sizes. Notice that
   declarations of vectors, arrays, and slices are defined explicitly
   elsewhere for word size 8 (for zero-termination). Higher-order
   modules would have been great here!
*)

(*---------------------------------------------*)
(*    Boolean Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgBool(type table = string)
                structure ArgA = TableArgBool(type table = chararray))
in
  (** SigDoc *)
  structure BoolVector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure BoolVectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure BoolArray : MONO_ARRAY = T.A
  (** SigDoc *)
  structure BoolArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure BoolArray2 : MONO_ARRAY2 = T.A2
end


(*---------------------------------------------*)
(*     Word16 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgWord16(type table = string)
                structure ArgA = TableArgWord16(type table = chararray))
in
  (** SigDoc *)
  structure Word16Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Word16VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Word16Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Word16ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Word16Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*     Word31 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgWord31(type table = string)
                structure ArgA = TableArgWord31(type table = chararray))
in
  (** SigDoc *)
  structure Word31Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Word31VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Word31Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Word31ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Word31Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*     Word32 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgWord32(type table = string)
                structure ArgA = TableArgWord32(type table = chararray))
in
  (** SigDoc *)
  structure Word32Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Word32VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Word32Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Word32ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Word32Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*     Word63 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

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


(*---------------------------------------------*)
(*     Word64 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

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

(*---------------------------------------------*)
(*     Word Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

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

(*---------------------------------------------*)
(*      Real Vectors, Arrays, and Slices       *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgReal(type table = string)
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
