
signature CALC_OFFSET =
  sig

    (* Insert offset annotations on SCOPE, LETREGION and HANDLE
       constructs in a LINE_STMT program after insertion of fetch
       and flush constructs.
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
    type StoreTypeIFF
    type offset = int
    type Atom

    datatype StoreType =
        STACK_STY of lvar * offset
      | PHREG_STY of lvar * lvar
      | FLUSHED_CALLEE_STY of lvar * offset
      | FLUSHED_CALLER_STY of lvar * lvar * offset
      | FV_STY of lvar * label * label

    val CO : {main_lab:label,code:(StoreTypeIFF,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list} ->
             {main_lab:label,code:(StoreType,offset,Atom) LinePrg,imports:label list * label list,exports:label list * label list}

    val CBV: {main_lab:label,code:(StoreType,offset,Atom) LinePrg,imports:label list * label list,exports:label list * label list} ->
             {main_lab:label,code:(StoreType,offset,Atom) LinePrg,imports:label list * label list,exports:label list * label list}

    val pr_sty    : StoreType -> string
    val pr_offset : offset -> string
    val pr_atom   : Atom -> string

  end
