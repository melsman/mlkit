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

    datatype StoreType =
      STACK_STY of lvar
    | PHREG_STY of lvar * int
    | NO_STY of lvar  (* NO_STY says that no sty has been assigned yet. *)

    datatype Atom =
      VAR           of lvar
    | RVAR          of place
    | DROPPED_RVAR  of place
    | PHREG         of int
    | INTEGER       of int 
    | UNIT

  and SimpleExp =
      ATOM            of Atom
    | FETCH           of label
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

    and LineStmt = 
      ASSIGN        of {pat: Atom, bind: SimpleExp}
    | FNJMP         of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | FNCALL        of {opr: Atom, args: Atom list, clos: Atom option, 
			free: Atom list, res: Atom list}
    | JMP           of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | FUNCALL       of {opr: label, args: Atom list, reg_vec: Atom option, 
			reg_args: Atom list, clos: Atom option, free: Atom list, res: Atom list}
    | LETREGION     of {rhos: binder list, body: LineStmt list}
    | SCOPE         of {pat: StoreType list, scope: LineStmt list}
    | HANDLE        of LineStmt list * LineStmt list
    | RAISE         of Atom
    | SWITCH_I      of int Switch
    | SWITCH_S      of string Switch
    | SWITCH_C      of (con*con_kind) Switch
    | SWITCH_E      of excon Switch
    | RESET_REGIONS of {force: bool, 
			regions_for_resetting: sma list}
    | CCALL         of {name: string,  
			args: Atom list,
			rhos_for_result : Atom list,
			res: Atom list}
    | FRAME         of {declared_lvars: {lvar: lvar, label: label} list,
			declared_excons: {excon: excon, label: label} list}

    and 'a Switch = SWITCH of Atom * ('a * (LineStmt list)) list * (LineStmt list)

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

    datatype TopDecl = 
      FUN of label * cc * LineStmt list
    | FN of label * cc * LineStmt list
  
    type LinePrg = TopDecl list

    val L : {main_lab:label,code:ClosPrg,imports:label list,exports:label list} -> 
            {main_lab:label,code:LinePrg,imports:label list,exports:label list}
    type StringTree
    val layout_line_prg : LinePrg -> StringTree

  end








