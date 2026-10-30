(* Table applications; keep slice and bounds helpers specialized here. *)

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

end

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

(* IntInf elements are GC-visible pointers, so LargeInt collections use the
 * polymorphic containers rather than packed fixed-width byte tables. *)
(** SigDoc *)
structure LargeIntVector : MONO_VECTOR where type elem = LargeInt.int =
struct
  open Vector
  type elem = LargeInt.int
  type vector = elem Vector.vector
end
(** SigDoc *)
structure LargeIntVectorSlice : MONO_VECTOR_SLICE
  where type elem = LargeInt.int and type vector = LargeIntVector.vector =
struct
  open VectorSlice
  type elem = LargeInt.int
  type vector = LargeIntVector.vector
  type slice = elem VectorSlice.slice
end
(** SigDoc *)
structure LargeIntArray : MONO_ARRAY
  where type elem = LargeInt.int and type vector = LargeIntVector.vector =
struct
  open Array
  type elem = LargeInt.int
  type array = elem Array.array
  type vector = LargeIntVector.vector
end
(** SigDoc *)
structure LargeIntArraySlice : MONO_ARRAY_SLICE
  where type elem = LargeInt.int and type array = LargeIntArray.array
    and type vector = LargeIntVector.vector and type vector_slice = LargeIntVectorSlice.slice =
struct
  open ArraySlice
  type elem = LargeInt.int
  type array = LargeIntArray.array
  type vector = LargeIntVector.vector
  type slice = elem ArraySlice.slice
  type vector_slice = LargeIntVectorSlice.slice
end
(** SigDoc *)
structure LargeIntArray2 : MONO_ARRAY2
  where type elem = LargeInt.int and type vector = LargeIntVector.vector =
struct
  open Array2
  type elem = LargeInt.int
  type array = elem Array2.array
  type vector = LargeIntVector.vector
  type region = elem Array2.region
end
