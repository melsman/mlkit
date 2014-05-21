structure Main =
  struct
    structure M3 = Main(Vector3);

    val name = "Barnes-Hut (3d)"

    fun testit strm = ()

    fun doit () = (
	  M3.srand 123;
	  M3.go {
	      output = fn {n2bcalc:int, nbccalc:int, nstep:int, selfint:int, tnow:real} => 
		 print ("{n2bcalc:" ^ Int.toString n2bcalc ^ ", nbccalc:" ^ Int.toString nbccalc ^
			", nstep:" ^ Int.toString nstep ^ ", selfint:" ^ Int.toString selfint ^ ", tnow:" ^
			Real.toString tnow ^ "}\n"),
	      bodies = M3.testdata 128,
	      tnow = 0.0, tstop = 2.0,
	      dtime = 0.025, eps = 0.05, tol = 1.0,
	      rmin = M3.S.V.tabulate (fn _ => ~2.0),
	      rsize = 4.0
	    })

  end

val _ = Main.doit()