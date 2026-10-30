(* Table applications; keep slice and bounds helpers specialized here. *)

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
