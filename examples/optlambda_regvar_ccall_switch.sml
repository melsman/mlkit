(* Test that the optimizer does not merge CCALLprim calls
   with different explicit region variables.

   We allocate different amounts into r1 and r2, then use a
   conditional to query one or the other.  If the optimizer
   merges the branches, both paths would return the usage of
   the same region.
*)

fun queryRegion (b : bool) =
    let with r1 r2
      val n1 = (Regions.getPageSizeBytes () div 2) + 1
      val n2 = (Regions.getPageSizeBytes () * 2) + 1
      val s1 : string`r1 = CharVector.tabulate (n1, fn _ => #"a")
      val s2 : string`r2 = CharVector.tabulate (n2, fn _ => #"b")
      val u1_direct = Regions.memoryUsageOfRegion `[r1] ()
      val u2_direct = Regions.memoryUsageOfRegion `[r2] ()
      val u_conditional =
        if b then prim `[r1] ("get_Region_Memory_Usage_Bytes", ())
             else prim `[r2] ("get_Region_Memory_Usage_Bytes", ())
    in
      if b then u_conditional = u1_direct
           else u_conditional = u2_direct
    end

val ok1 = queryRegion true
val ok2 = queryRegion false

val _ =
  if ok1 andalso ok2
  then print "OK: optlambda_regvar_ccall_switch\n"
  else (if not ok1
        then print "FAIL: optlambda_regvar_ccall_switch (true branch)\n"
        else ();
        if not ok2
        then print "FAIL: optlambda_regvar_ccall_switch (false branch)\n"
        else ())
