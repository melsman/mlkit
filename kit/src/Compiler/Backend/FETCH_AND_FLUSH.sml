
signature FETCH_AND_FLUSH =
  sig

    (* Insert Fetch and Flush instructions in a LINE_STMT program
       after register allocation.

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
    type phreg
    type StoreTypeRA
    type Atom

    datatype StoreType =
        STACK_STY of lvar
      | PHREG_STY of lvar * phreg
      | FLUSHED_CALLEE_STY of phreg
      | FLUSHED_CALLER_STY of lvar * phreg

    val IFF : {main_lab:label,code:(StoreTypeRA,unit,Atom) LinePrg,imports:label list,exports:label list} ->
              {main_lab:label,code:(StoreType,unit,Atom) LinePrg,imports:label list,exports:label list}

    val pr_sty    : StoreType -> string
    val pr_atom   : Atom -> string

  end








