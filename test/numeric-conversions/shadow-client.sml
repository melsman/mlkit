val () =
    if narrowShadow (IntInf.fromInt 23) = Int32.fromInt 23 then
      print "conversion shadowing: OK\n"
    else raise Fail "cross-unit conversion shadowing"
