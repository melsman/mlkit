signature CODE_GEN_KAM =
  sig
    type label
    type AsmPrg
    type ClosPrg

    val CG : {main_lab_opt:label option,
	      code: ClosPrg,
	      imports:label list * label list,
	      exports:label list * label list} -> AsmPrg
  end