(* Table applications; keep slice and bounds helpers specialized here. *)

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

local
  structure T = BuildTables(structure ArgV = TableArgReal16(type table = string)
                            structure ArgA = TableArgReal16(type table = chararray))
in
  (** SigDoc *)
  structure Real16Vector : MONO_VECTOR = T.V
  (** SigDoc *)
  structure Real16VectorSlice : MONO_VECTOR_SLICE = T.VS
  (** SigDoc *)
  structure Real16Array : MONO_ARRAY = T.A
  (** SigDoc *)
  structure Real16ArraySlice : MONO_ARRAY_SLICE = T.AS
  (** SigDoc *)
  structure Real16Array2 : MONO_ARRAY2 = T.A2
end
