
signature SUBST_AND_SIMPLIFY =
  sig

    (* Simplify the program...

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
    type StoreTypeCO
    type AtomCO
    type offset = int

    datatype Aty =
        REG_I_ATY        of offset
      | REG_F_ATY        of offset
      | STACK_ATY        of offset
      | DROPPED_RVAR_ATY
      | PHREG_ATY        of reg
      | INTEGER_ATY      of int 
      | UNIT_ATY

    val SS : {main_lab:label,code:(StoreTypeCO,offset,AtomCO) LinePrg,imports:label list,exports:label list} ->
             {main_lab:label,code:(StoreTypeCO,offset,Aty) LinePrg,imports:label list,exports:label list}


  end








