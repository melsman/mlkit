
signature FETCH_AND_FLUSH =
  sig

    (* Insert Fetch and Flush instructions in a LINE_STMT program
       after register allocation. *)

    type place
    type phsize
    type pp = int
    type lvar
    type con
    type excon
    type cc
    type label
    type ('sty,'offset,'aty) LinePrg
    type StoreTypeRA
    type Atom

    datatype StoreType =
        STACK_STY of lvar
      | PHREG_STY of lvar * lvar
      | FLUSHED_CALLEE_STY of lvar
      | FLUSHED_CALLER_STY of lvar * lvar
      | FV_STY             of lvar * label * label

    val IFF : {main_lab:label,code:(StoreTypeRA,unit,Atom) LinePrg,
               imports:label list * label list,exports:label list * label list} ->
              {main_lab:label,code:(StoreType,unit,Atom) LinePrg,
               imports:label list * label list,exports:label list * label list}

    val pr_sty    : StoreType -> string
    val pr_atom   : Atom -> string

  end
