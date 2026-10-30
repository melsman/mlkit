(* Table applications; keep slice and bounds helpers specialized here. *)

local
  structure T =
    BuildTables(structure ArgV = TableArgInt8(type table = string)
                structure ArgA = TableArgInt8(type table = chararray))
in
  (** SigDoc *)
  structure Int8Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int8VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int8Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int8ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int8Array2 : MONO_ARRAY2 = T.A2
end

local
  structure T =
    BuildTables(structure ArgV = TableArgInt16(type table = string)
                structure ArgA = TableArgInt16(type table = chararray))
in
  (** SigDoc *)
  structure Int16Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Int16VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Int16Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Int16ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Int16Array2 : MONO_ARRAY2 = T.A2
end

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
