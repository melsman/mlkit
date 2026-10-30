(* Table applications; keep slice and bounds helpers specialized here. *)

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
