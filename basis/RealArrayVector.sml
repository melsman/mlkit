
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
structure RealVector = Tables.RealVector
structure RealArray = Tables.RealArray
structure RealArray2 = Tables.RealArray2
end

structure Real64Vector = RealVector
structure Real64Array = RealArray
structure Real64Array2 = RealArray2
structure LargeRealVector = RealVector
structure LargeRealArray = RealArray
structure LargeRealArray2 = RealArray2
