
signature REG_ALLOC =
  sig

    (* Register allocation uses the language LINE_STMT.

       The register allocator inserts a store type on all scope
       constructs which says wheter a variable is mapped into a
       machine register or is spilled on the stack.

       The function CC_ls makes sure that all variables that are
       potentially spilled are introduced by a scope construct. Call
       conventions are also made explicit around calls by binding
       values to machine registers. The function CC_ls is used before
       register allocation.

       The function ra_dummy performs a dummy register allocation
       where all variables are spilled.

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
    type Atom

    datatype StoreType =
      STACK_STY of lvar
    | PHREG_STY of lvar * lvar

    val ra_dummy : {main_lab:label,code:(lvar,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list} ->
                   {main_lab:label,code:(StoreType,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list}

    val ra       : {main_lab:label,code:(lvar,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list} ->
                   {main_lab:label,code:(StoreType,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list}

    val pr_sty    : StoreType -> string
    val pr_atom   : Atom -> string

  end








