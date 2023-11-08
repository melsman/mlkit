val a = Int32.maxInt
val b = List.tabulate(4, fn i => i)
val c = 1234567890123456789012345678901234567890
val d = c * c
val e = IntInf.toString d
val f = SOME {x = 34, y = 34.2}
val g = case f of SOME r => Real.toString (#y r) | _ => "NO";
:quit;
