
signature CODE_GEN =
  sig

    (* Generate Target Code

    *)

    type place 
    type phsize
    type pp = int
    type lvar
    type con
    type excon
    type cc
    type label
    type ('sty,'offset,'aty) LinePrg
    type reg
    type offset = int
    type StoreTypeSS
    type AtySS
    type RiscPrg

    val CG   : {main_lab:label,code:(StoreTypeSS,offset,AtySS) LinePrg,imports:label list,exports:label list} -> RiscPrg
    val generate_link_code : label list -> RiscPrg
    val emit : RiscPrg * string -> unit
  end








