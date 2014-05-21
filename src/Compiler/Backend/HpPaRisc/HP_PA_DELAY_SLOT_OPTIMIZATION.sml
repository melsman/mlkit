signature HP_PA_DELAY_SLOT_OPTIMIZATION =
  sig

  (* ----------------------------------------------------------------------
   * Delay slot optimization for HP Precision Architecture code. 
   * ---------------------------------------------------------------------- *)

   type AsmPrg
   val DSO : AsmPrg -> AsmPrg

  end
