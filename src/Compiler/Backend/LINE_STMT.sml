signature LINE_STMT =
  sig

    (* Next language after CLOS_EXP is LINE_STMT:

       The linearization phase linearises the code and inserts 
             scope lvar in stmts end
       constructs such that the scope of variables are maintained.

       The function L linearises the code. 

       The language LINE_STMT is used by the following phases:
         
         - Register allocation (module RegAlloc.sml)
         - Fetch and Flush (module FetchFlush.sml)
         - Calculate Offsets (module CalcOffsets.sml)
         - Substitution and Simplify (module SubstAndSimplify.sml)
         - Code generation (module CodeGen.sml)

    *)

    type place 
    type phsize
    type pp = int
    type lvar
    type con
    type excon
    type cc
    type label
    type ClosPrg

    datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | BOXED of int

    type binder = place * phsize
    type phreg = word
    datatype Atom =
      VAR           of lvar
    | RVAR          of place
    | DROPPED_RVAR  of place
    | PHREG         of phreg
    | INTEGER       of int 
    | UNIT

  and SimpleExp =
      ATOM            of Atom
    | LOAD            of label
    | STORE           of Atom * label
    | STRING          of string
    | REAL            of string
    | CLOS_RECORD     of {label: label, elems: Atom list, alloc: sma}
    | REGVEC_RECORD   of {elems: sma list, alloc: sma}
    | SCLOS_RECORD    of {elems: Atom list, alloc: sma}
    | RECORD          of {elems: Atom list, alloc: sma}
    | SELECT          of int * Atom
    | CON0            of {con: con, con_kind: con_kind, aux_regions: sma list, alloc: sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: sma, arg: Atom}
    | DECON           of {con: con, con_kind: con_kind, con_atom: Atom}
    | DEREF           of Atom
    | REF             of sma * Atom
    | ASSIGNREF       of sma * Atom * Atom
    | PASS_PTR_TO_MEM of sma * int (* Used only by CCALL *)
    | PASS_PTR_TO_RHO of sma       (* Used only by CCALL *)

    and ('sty,'offset) LineStmt = 
      ASSIGN        of {pat: Atom, bind: SimpleExp}
    | FLUSH         of Atom * 'offset
    | FETCH         of Atom * 'offset
    | FNJMP         of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | FNCALL        of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | JMP           of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | FUNCALL       of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | LETREGION     of {rhos: (binder*'offset) list, body: ('sty,'offset) LineStmt list}
    | SCOPE         of {pat: 'sty list, scope: ('sty,'offset) LineStmt list}
    | HANDLE        of ('sty,'offset) LineStmt list * ('sty,'offset) LineStmt list * 'offset
    | RAISE         of {arg: Atom,defined_atoms: Atom list}
    | SWITCH_I      of (int,'sty,'offset) Switch
    | SWITCH_S      of (string,'sty,'offset) Switch
    | SWITCH_C      of ((con*con_kind),'sty,'offset) Switch
    | SWITCH_E      of (excon,'sty,'offset) Switch
    | RESET_REGIONS of {force: bool, 
			regions_for_resetting: sma list}
    | CCALL         of {name: string,  
			args: Atom list,
			rhos_for_result : Atom list,
			res: Atom list}

    and ('a,'sty,'offset) Switch = SWITCH of Atom * ('a * (('sty,'offset) LineStmt list)) list * (('sty,'offset) LineStmt list)

    and sma = 
      ATTOP_LI of Atom * pp
    | ATTOP_LF of Atom * pp
    | ATTOP_FI of Atom * pp
    | ATTOP_FF of Atom * pp
    | ATBOT_LI of Atom * pp
    | ATBOT_LF of Atom * pp
    | SAT_FI   of Atom * pp
    | SAT_FF   of Atom * pp
    | IGNORE

    datatype ('sty,'offset) TopDecl = 
      FUN of label * cc * ('sty,'offset) LineStmt list
    | FN of label * cc * ('sty,'offset) LineStmt list
  
    type ('sty,'offset) LinePrg = ('sty,'offset) TopDecl list

    val L : {main_lab:label,code:ClosPrg,imports:label list,exports:label list} -> 
            {main_lab:label,code:(lvar,unit) LinePrg,imports:label list,exports:label list}

    (*********************************)
    (* Def and Use sets for LineStmt *)
    (*********************************)
    val use_ls            : ('sty,'offset) LineStmt -> lvar list
    val def_ls            : ('sty,'offset) LineStmt -> lvar list
    val def_use_ls        : ('sty,'offset) LineStmt -> lvar list * lvar list
    val get_lvar_atom     : Atom * lvar list -> lvar list
    val get_lvar_atoms    : Atom list * lvar list -> lvar list
    val get_lvar_atom_opt : Atom option * lvar list -> lvar list
    val get_lvar_sma      : sma * lvar list -> lvar list
    val get_lvar_smas     : sma list * lvar list -> lvar list
    val use_on_fn         : {opr: Atom,args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> lvar list
    val def_on_fn         : {opr: Atom,args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> lvar list
    val use_on_fun        : {opr: label,args: Atom list,reg_vec: Atom option,reg_args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> lvar list
    val def_on_fun        : {opr: label,args: Atom list,reg_vec: Atom option,reg_args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> lvar list

    (*****************************************)
    (* Get Machine Registers from a LineStmt *)
    (*****************************************)
    val get_phreg_atom     : Atom * phreg list -> phreg list
    val get_phreg_atoms    : Atom list * phreg list -> phreg list
    val get_phreg_atom_opt : Atom option * phreg list -> phreg list
    val get_phreg_sma      : sma * phreg list -> phreg list
    val get_phreg_smas     : sma list * phreg list -> phreg list
    val get_phreg_in_fun   : {opr: label,args: Atom list,reg_vec: Atom option,reg_args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> phreg list
    val get_phreg_in_fn    : {opr: Atom,args: Atom list,clos: Atom option,free: Atom list,res: Atom list} -> phreg list
    val get_phreg_ls       : ('sty,'offset) LineStmt -> phreg list

    type StringTree
    val layout_line_prg : ('sty -> string) -> ('offset -> string) -> ('sty,'offset) LinePrg -> StringTree

  end








