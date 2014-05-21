signature HPPA_RESOLVE_JUMPS =
  sig

  (* ----------------------------------------------------------------------
   * Resolvation of jumps for HP Precision Architecture code. 
   * ---------------------------------------------------------------------- *)

   type AsmPrg
   val RJ : AsmPrg -> AsmPrg

  end
