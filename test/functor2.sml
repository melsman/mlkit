signature R = sig type r(*58*) val R : r end

functor A(R : R (*59*)) =
  struct
    type r(*59*) = R.r
  end

functor B(R : R (*60*)) =
  struct
    structure A  = A(R)  (*{r(60) -> /\().r(E 63), r(59) -> /\().r(E 62)}*)
    type r(*60*) = A.r
  end

functor C() = 
  struct
    structure R = struct datatype r(*62*) = R end
    structure A  = A(R)  (*{r(E 62) -> r(E 63), r(59) -> /\().r(E 63)}*)

  end

structure C(*63*) = C()   (*{r(E 62) -> r(E 63)}: ok*)
structure B(*63*) = B(C.R)  (*{r(60) -> /\().r(E 63), r(59) -> /\().r(E 62)}*)

