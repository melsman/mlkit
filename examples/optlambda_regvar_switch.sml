(* Test that the optimizer does not merge resetRegions calls
   with different explicit region variables.

   If OptLambda's eq_prim considers two RESET_REGIONSprim equal
   when they differ only in regvars, the switch elimination
   optimization could collapse the two branches, resetting the
   wrong region.

   We test both branches:
   - resetFirst true  should reset r1, leave r2 unchanged
   - resetFirst false should reset r2, leave r1 unchanged
*)

fun resetFirst (b : bool) =
    let with r1 r2
      val n = (Regions.getPageSizeBytes () div 2) + 1
      val s1 : string`r1 = CharVector.tabulate (n, fn _ => #"a")
      val s2 : string`r2 = CharVector.tabulate (n, fn _ => #"b")
      val u1_before = Regions.memoryUsageOfRegion `[r1] ()
      val u2_before = Regions.memoryUsageOfRegion `[r2] ()
      val _ = if b then forceResetting `[r1] ()
                   else forceResetting `[r2] ()
      val u1_after = Regions.memoryUsageOfRegion `[r1] ()
      val u2_after = Regions.memoryUsageOfRegion `[r2] ()
    in
      if b then
        (* r1 should have been reset, r2 should not *)
        u1_after < u1_before andalso u2_after = u2_before
      else
        (* r2 should have been reset, r1 should not *)
        u2_after < u2_before andalso u1_after = u1_before
    end

val ok1 = resetFirst true
val ok2 = resetFirst false

val _ =
  if ok1 andalso ok2
  then print "OK: optlambda_regvar_switch\n"
  else (if not ok1
        then print "FAIL: optlambda_regvar_switch (true branch)\n"
        else ();
        if not ok2
        then print "FAIL: optlambda_regvar_switch (false branch)\n"
        else ())
