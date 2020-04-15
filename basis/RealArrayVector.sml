
local
  structure Tables :> sig
     structure RealVector : MONO_VECTOR
     structure RealArray : MONO_ARRAY
     sharing type RealVector.vector = RealArray.vector
  end where type RealVector.elem = real
      where type RealArray.elem = real
  = struct
       structure RealVector : MONO_VECTOR =
       struct open RealTable
              val update = updatev
       end
       structure RealArray : MONO_ARRAY = RealTable
    end
in
structure RealVector = Tables.RealVector
structure RealArray = Tables.RealArray
end

structure Real64Vector = RealVector
structure Real64Array = RealArray
structure LargeRealVector = RealVector
structure LargeRealArray = RealArray
