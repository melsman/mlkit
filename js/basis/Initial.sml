structure Initial =
  struct
    type int0 = int
    type word0 = word

    (* Real structure *)
    local
      fun get_posInf () : real = prim ("posInfFloat", ())
      fun get_negInf () : real = prim ("negInfFloat", ())
    in
      val posInf = get_posInf()
      val negInf = get_negInf()
    end

    (* Math structure *)
    local
      fun sqrt (r : real) : real = prim ("sqrtFloat", r)
      fun ln' (r : real) : real = prim ("lnFloat", r)
    in
      val ln10 = ln' 10.0 
      val NaN = sqrt ~1.0
    end

  end
