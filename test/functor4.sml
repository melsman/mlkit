signature R = sig type r(*58*) val R : r end

functor A(R : R (*59*)) =
  struct
    type r(*59*) = R.r
  end

structure R = struct datatype r(*62*) = R end

structure B = A(R)

