
signature CALC_OFFSET =
  sig

    (* Insert offset annotations on SCOPE, LETREGION and HANDLE
       constructs in in a LINE_STMT program after insertion of fetch
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
    type ('sty,'offset) LinePrg
    type phreg = word
    type StoreTypeIFF
    type offset = int

    datatype StoreType =
        STACK_STY of lvar * offset
      | PHREG_STY of lvar * phreg
      | FLUSHED_CALLEE_STY of phreg * offset
      | FLUSHED_CALLER_STY of lvar * phreg * offset

    val CO : {main_lab:label,code:(StoreTypeIFF,unit) LinePrg,imports:label list,exports:label list} ->
             {main_lab:label,code:(StoreType,offset) LinePrg,imports:label list,exports:label list}

    val pr_sty    : StoreType -> string
    val pr_offset : offset -> string

  end








