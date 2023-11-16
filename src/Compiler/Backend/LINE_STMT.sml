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
    datatype phsize = INF | WORDS of int
    type pp = int
    type lvar
    type con
    type excon
    type cc
    type label
    type ClosPrg

    (* for use with the CCALL_AUTO construct: *)
    datatype foreign_type =
      CharArray               (* When passed to a c-function, the
			       * data field of the ML char-array is
			       * passed. Char-arrays in the Kit are
			       * zero-terminated and prefixed
                               * with a size-tag. CharArray may
			       * not appear in a function result
                               * type. *)
    | ForeignPtr              (* Foreign pointers are marked so
			       * that the garbage collector does
			       * not trace such pointers. *)
    | Bool                    (* Booleans are always tagged and
			       * must thus be converted when
			       * passed between the C world and
			       * the ML world. *)
    | Int                     (* Integers (and words) are tagged
			       * when garbage collection is
			       * enabled. *)
    | Unit                    (* Possible in results *)

    datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | UNBOXED_HIGH of int
    | BOXED of int

    type binder = place * phsize

    datatype Atom =
      VAR           of lvar
    | FLOW_VAR      of lvar * label * label
    | RVAR          of place
    | DROPPED_RVAR  of place
    | PHREG         of lvar
    | INTEGER       of {value:IntInf.int, precision:int}
    | WORD          of {value:IntInf.int, precision:int}
    | UNIT

    datatype StoreType =
      V of lvar
    | FV of lvar * label * label

    datatype 'aty SimpleExp =
      ATOM            of 'aty
    | LOAD            of label
    | STORE           of 'aty * label (* moved to LineStmt??? 2001-03-15, Niels *)
    | STRING          of string
    | REAL            of string
    | F64             of string
    | CLOS_RECORD     of {label: label, elems: 'aty list*'aty list*'aty list, f64_vars: int, alloc: 'aty sma}
    | REGVEC_RECORD   of {elems: 'aty sma list, alloc: 'aty sma}
    | SCLOS_RECORD    of {elems: 'aty list*'aty list*'aty list, f64_vars: int, alloc: 'aty sma}
    | RECORD          of {elems: 'aty list, alloc: 'aty sma, tag: Word32.word, maybeuntag: bool}
    | BLOCKF64        of {elems: 'aty list, alloc: 'aty sma, tag: Word32.word}
    | SCRATCHMEM      of {bytes: int, alloc: 'aty sma, tag: Word32.word}
    | SELECT          of int * 'aty
    | CON0            of {con: con, con_kind: con_kind, aux_regions: 'aty sma list, alloc: 'aty sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: 'aty sma, arg: 'aty}
    | DECON           of {con: con, con_kind: con_kind, con_aty: 'aty}
    | DEREF           of 'aty
    | REF             of 'aty sma * 'aty
    | ASSIGNREF       of 'aty sma * 'aty * 'aty
    | PASS_PTR_TO_MEM of 'aty sma * int * bool (* Used only by CCALL *)
                                               (* The boolean is true if the region has an untagged type *)
    | PASS_PTR_TO_RHO of 'aty sma              (* Used only by CCALL *)

    and ('sty,'offset,'aty) LineStmt =
      ASSIGN        of {pat: 'aty, bind: 'aty SimpleExp}
    | FLUSH         of 'aty * 'offset
    | FETCH         of 'aty * 'offset
    | FNJMP         of {opr: 'aty, args: 'aty list, clos: 'aty option,
			res: 'aty list, bv: Word32.word list}
    | FNCALL        of {opr: 'aty, args: 'aty list, clos: 'aty option,
			res: 'aty list, bv: Word32.word list}
    | JMP           of {opr: label, args: 'aty list,
			reg_args: 'aty list, fargs: 'aty list, clos: 'aty option, res: 'aty list, bv: Word32.word list}
    | FUNCALL       of {opr: label, args: 'aty list,
			reg_args: 'aty list, fargs: 'aty list, clos: 'aty option, res: 'aty list, bv: Word32.word list}
    | LETREGION     of {rhos: (binder*'offset) list, body: ('sty,'offset,'aty) LineStmt list}
    | SCOPE         of {pat: 'sty list, scope: ('sty,'offset,'aty) LineStmt list}
    | HANDLE        of {default: ('sty,'offset,'aty) LineStmt list,
			handl: ('sty,'offset,'aty) LineStmt list * 'aty,
			handl_return: ('sty,'offset,'aty) LineStmt list * 'aty * (Word32.word list),
			offset: 'offset}
    | RAISE         of {arg: 'aty,defined_atys: 'aty list}
    | SWITCH_I      of {switch: (IntInf.int,'sty,'offset,'aty) Switch, precision: int}
    | SWITCH_W      of {switch: (IntInf.int,'sty,'offset,'aty) Switch, precision: int}
    | SWITCH_S      of (string,'sty,'offset,'aty) Switch
    | SWITCH_C      of ((con*con_kind),'sty,'offset,'aty) Switch
    | SWITCH_E      of (excon,'sty,'offset,'aty) Switch
    | RESET_REGIONS of {force: bool, regions_for_resetting: 'aty sma list}
    | PRIM          of {name: PrimName.prim, args: 'aty list, res: 'aty list}
    | CCALL         of {name: string, args: 'aty list,
			rhos_for_result : 'aty list, res: 'aty list}
    | CCALL_AUTO    of {name: string, args: ('aty * foreign_type) list,
			res: 'aty * foreign_type}
    | EXPORT        of {name: string, clos_lab: label, arg: 'aty * foreign_type * foreign_type}

    and ('a,'sty,'offset,'aty) Switch = SWITCH of 'aty * ('a * (('sty,'offset,'aty) LineStmt list)) list * (('sty,'offset,'aty) LineStmt list)

    and 'aty sma =
      ATTOP_LI of 'aty * pp
    | ATTOP_LF of 'aty * pp
    | ATTOP_FI of 'aty * pp
    | ATTOP_FF of 'aty * pp
    | ATBOT_LI of 'aty * pp
    | ATBOT_LF of 'aty * pp
    | SAT_FI   of 'aty * pp
    | SAT_FF   of 'aty * pp
    | IGNORE

    datatype ('sty,'offset,'aty) TopDecl =
      FUN of label * cc * ('sty,'offset,'aty) LineStmt list
    | FN of label * cc * ('sty,'offset,'aty) LineStmt list

    type ('sty,'offset,'aty) LinePrg = ('sty,'offset,'aty) TopDecl list

    val smash_free : 'aty list*'aty list*'aty list -> 'aty list

    val L : {main_lab:label,code:ClosPrg,imports:label list * label list,exports:label list * label list} ->
            {main_lab:label,code:(StoreType,unit,Atom) LinePrg,imports:label list * label list,exports:label list * label list}

    val map_lss : ('aty1 -> 'aty2) -> ('offset1 -> 'offset2) -> ('sty1 -> 'sty2) ->
                  ('sty1,'offset1,'aty1) LineStmt list ->
                  ('sty2,'offset2,'aty2) LineStmt list
    val map_prg : ('aty1 -> 'aty2) -> ('offset1 -> 'offset2) -> ('sty1 -> 'sty2) ->
                  {main_lab:label,code:('sty1,'offset1,'aty1) LinePrg,imports:label list * label list,exports:label list * label list} ->
                  {main_lab:label,code:('sty2,'offset2,'aty2) LinePrg,imports:label list * label list,exports:label list * label list}

    val allocating : ('sty,'offset,'aty) LineStmt list -> bool
(*
    (*****************************************************************)
    (* In CalcOffsets we must know if a region has runtime type REAL *)
    (*****************************************************************)
    val is_region_real : place -> bool
*)

    (***************************************************)
    (* Def and Use sets for LineStmt RETURN lvars ONLY *)
    (***************************************************)
    val get_lvar_atom     : Atom * lvar list -> lvar list
    val get_lvar_atoms    : Atom list * lvar list -> lvar list
    val get_lvar_atom_opt : Atom option * lvar list -> lvar list
    val get_lvar_sma      : Atom sma * lvar list -> lvar list
    val get_lvar_smas     : Atom sma list * lvar list -> lvar list

    val use_lvar_on_fn    : {opr: Atom,args: Atom list,clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val def_lvar_on_fn    : {opr: Atom,args: Atom list,clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val use_lvar_on_fun   : {opr: label,args: Atom list,reg_args: Atom list, fargs: Atom list,
			     clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val def_lvar_on_fun   : {opr: label,args: Atom list,reg_args: Atom list, fargs: Atom list,
			     clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list

    val use_lvar_ls       : ('sty,'offset,Atom) LineStmt -> lvar list
    val def_lvar_ls       : ('sty,'offset,Atom) LineStmt -> lvar list
    val def_use_lvar_ls   : ('sty,'offset,Atom) LineStmt -> lvar list * lvar list

    (**************************************************************)
    (* Def and Use sets for LineStmt RETURN BOTH lvars and phregs *)
    (**************************************************************)
    val get_var_atom     : Atom * lvar list -> lvar list
    val get_var_atoms    : Atom list * lvar list -> lvar list
    val get_var_atom_opt : Atom option * lvar list -> lvar list
    val get_var_sma      : Atom sma * lvar list -> lvar list
    val get_var_smas     : Atom sma list * lvar list -> lvar list

    val use_var_on_fn    : {opr: Atom,args: Atom list,clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val def_var_on_fn    : {opr: Atom,args: Atom list,clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val use_var_on_fun   : {opr: label,args: Atom list,reg_args: Atom list, fargs: Atom list,
			    clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val def_var_on_fun   : {opr: label,args: Atom list,reg_args: Atom list, fargs: Atom list,
			    clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list

    val use_var_ls       : ('sty,'offset,Atom) LineStmt -> lvar list
    val def_var_ls       : ('sty,'offset,Atom) LineStmt -> lvar list
    val def_use_var_ls   : ('sty,'offset,Atom) LineStmt -> lvar list * lvar list

    (**************************************************************)
    (* Def and Use sets for LineStmt RETURN only lvars            *)
    (* Lvars bound to regions are also filtered out; they are not *)
    (* part of the root-set and are therefore not included in the *)
    (* bit vectors. Also lvars bound to unboxed f64 float values  *)
    (* are filtered out (for calc bit-vectors).                   *)
    (**************************************************************)
    val def_use_var_ls_cbv : ('sty,'offset,Atom) LineStmt -> lvar list * lvar list

    (*****************************************)
    (* Get Machine Registers from a LineStmt *)
    (*****************************************)
    val get_phreg_atom     : Atom * lvar list -> lvar list
    val get_phreg_atoms    : Atom list * lvar list -> lvar list
    val get_phreg_atom_opt : Atom option * lvar list -> lvar list
    val get_phreg_sma      : Atom sma * lvar list -> lvar list
    val get_phreg_smas     : Atom sma list * lvar list -> lvar list
    val get_phreg_in_fun   : {opr: label,args: Atom list,reg_args: Atom list, fargs: Atom list,
			      clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val get_phreg_in_fn    : {opr: Atom,args: Atom list,clos: Atom option,res: Atom list,bv: Word32.word list} -> lvar list
    val get_phreg_ls       : ('sty,'offset,Atom) LineStmt -> lvar list

    type StringTree
    val layout_line_prg : ('sty -> string) -> ('offset -> string) -> ('aty -> string) -> bool -> ('sty,'offset,'aty) LinePrg -> StringTree
    val layout_line_stmt: ('sty -> string) -> ('offset -> string) -> ('aty -> string) -> bool -> ('sty,'offset,'aty) LineStmt -> StringTree
    val pr_line_stmt    : ('sty -> string) -> ('offset -> string) -> ('aty -> string) -> bool -> ('sty,'offset,'aty) LineStmt -> string
    val pr_phreg        : lvar -> string
    val pr_atom         : Atom -> string
    val pr_sty          : StoreType -> string
  end
