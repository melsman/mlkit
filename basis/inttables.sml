(* Applications of the WordTable and WordSlice functors (defined in
   wordtable-functors.sml) for various word-sizes to implement
   monomorphic vectors, arrays, slices, and 2-dimensional arrays with
   integers as elements.
*)

(*---------------------------------------------*)
(*      Int31 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt31(type table = string)
                structure ArgA = TableArgInt31(type table = chararray))
in
  (** SigDoc *)
  structure Int31Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int31VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int31Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int31ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int31Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*      Int32 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt32(type table = string)
                structure ArgA = TableArgInt32(type table = chararray))
in
  (** SigDoc *)
  structure Int32Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int32VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int32Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int32ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int32Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*      Int63 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt63(type table = string)
                structure ArgA = TableArgInt63(type table = chararray))
in
  (** SigDoc *)
  structure Int63Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int63VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int63Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int63ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int63Array2 : MONO_ARRAY2 = T.A2
end

(*---------------------------------------------*)
(*      Int64 Vectors, Arrays, and Slices      *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt64(type table = string)
                structure ArgA = TableArgInt64(type table = chararray))
in
  (** SigDoc *)
  structure Int64Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int64VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int64Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int64ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int64Array2 : MONO_ARRAY2 = T.A2
  (** SigDoc *)
  structure LargeIntVector : MONO_VECTOR = Word64Vector
  (** SigDoc *)
  structure LargeIntVectorSlice : MONO_VECTOR_SLICE = Word64VectorSlice
  (** SigDoc *)
  structure LargeIntArray : MONO_ARRAY = Word64Array
  (** SigDoc *)
  structure LargeIntArraySlice : MONO_ARRAY_SLICE = Word64ArraySlice
  (** SigDoc *)
  structure LargeIntArray2 : MONO_ARRAY2 = Word64Array2
end

(*---------------------------------------------*)
(*       Int Vectors, Arrays, and Slices       *)
(*---------------------------------------------*)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt(type table = string)
                structure ArgA = TableArgInt(type table = chararray))
in
  (** SigDoc *)
  structure IntVector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure IntVectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure IntArray : MONO_ARRAY = T.A
  (** SigDoc *)
  structure IntArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure IntArray2 : MONO_ARRAY2 = T.A2
end
