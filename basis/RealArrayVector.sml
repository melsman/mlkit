
local
  structure Tables :> sig
     structure RealVector : MONO_VECTOR
     structure RealArray : MONO_ARRAY
     sharing type RealVector.vector = RealArray.vector
     structure RealArray2 : MONO_ARRAY2
     sharing type RealArray2.vector = RealVector.vector
  end where type RealVector.elem = real
      where type RealArray.elem = real
      where type RealArray2.elem = real
  = struct
       structure RealVector : MONO_VECTOR =
       struct open RealTable
              val update = updatev
       end
       structure RealArray : MONO_ARRAY = RealTable
       structure RealArray2 : MONO_ARRAY2 = RealArray2
    end
in
(** SigDoc *)
structure RealVector : MONO_VECTOR = Tables.RealVector

(** SigDoc *)
structure RealArray : MONO_ARRAY = Tables.RealArray

(** SigDoc *)
structure RealArray2 : MONO_ARRAY2 = Tables.RealArray2
end

(** SigDoc *)
structure Real64Vector : MONO_VECTOR = RealVector

(** SigDoc *)
structure Real64Array : MONO_ARRAY = RealArray

(** SigDoc *)
structure Real64Array2 : MONO_ARRAY2 = RealArray2

(** SigDoc *)
structure LargeRealVector : MONO_VECTOR = RealVector

(** SigDoc *)
structure LargeRealArray : MONO_ARRAY = RealArray

(** SigDoc *)
structure LargeRealArray2 : MONO_ARRAY2 = RealArray2
