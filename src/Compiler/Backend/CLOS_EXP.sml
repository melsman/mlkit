
signature CLOS_EXP =
  sig

    (* Next language after MUL_EXP is CLOS_EXP:
       All functions are now at top level, and the free variables
       are bound in each function.

       The module ClosExp translate MUL_EXP into CLOS_EXP and performs
       closure conversion.

         1- The function N normalizes a MulExp program such that all
            functions are bound to a variable; an extension to K normal
            form. The result is a MulExp program

         2- The function F splits the set of functions in functions
            implemented with closures and functions implemented
            without closures. The result is an mapping Fenv mapping
            functions that may be implemented without a closure into
            the free variables of the function. If a function is not
            in the domain of Fenv then it must be implemented with a
            closure

         3- The function clos_conv performs closure conversion and
            translates a MulExp program into a ClosExp program.

         4- The function cc activates the above three functions.

       The next language after CLOS_EXP is LINE_EXP (Linearized code)
    *)

    type ('a,'b,'c)LambdaPgm
    type place
    type 'a at
    type phsize

    type pp = int
    type lvar
    type con
    type excon

    type cc
    type label

    datatype foreign_type = CharArray | ForeignPtr | Bool | Int | Int32 | Int64 | Unit

    datatype con_kind =  (* the integer is the index in the datatype 0,... *)
      ENUM of int
    | UNBOXED of int
    | UNBOXED_HIGH of int
    | BOXED of int

    type binder = place * phsize

    datatype ClosExp =
      VAR             of lvar
    | RVAR            of {rho:place}
    | DROPPED_RVAR    of {rho:place}
    | FETCH           of label
    | STORE           of ClosExp * label
    | INTEGER         of {value: IntInf.int, precision: int}
    | WORD            of {value: IntInf.int, precision: int}
    | STRING          of string
    | REAL            of string
    | F64             of string
    | PASS_PTR_TO_MEM of sma * int
    | PASS_PTR_TO_RHO of {sma: sma}
    | UB_RECORD       of ClosExp list
    | CLOS_RECORD     of {label: label, elems: ClosExp list * ClosExp list * ClosExp list, f64_vars: int, alloc: sma}
    | REGVEC_RECORD   of {elems: sma list, alloc: sma}
    | SCLOS_RECORD    of {elems: ClosExp list * ClosExp list * ClosExp list, f64_vars: int, alloc: sma}
    | RECORD          of {elems: ClosExp list, alloc: sma, tag: Word32.word, maybeuntag: bool}
    | BLOCKF64        of {elems: ClosExp list, alloc: sma, tag: Word32.word}
    | SCRATCHMEM      of {bytes: int, alloc: sma, tag: Word32.word}
    | SELECT          of int * ClosExp
    | FNJMP           of {opr: ClosExp, args: ClosExp list, clos: ClosExp option}
    | FNCALL          of {opr: ClosExp, args: ClosExp list, clos: ClosExp option}
    | JMP             of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list,
                          clos: ClosExp option}
    | FUNCALL         of {opr: label, args: ClosExp list, reg_vec: ClosExp option, reg_args: ClosExp list,
                          clos: ClosExp option}
    | LETREGION       of {rhos: binder list, body: ClosExp}
    | LET             of {pat: lvar list, bind: ClosExp, scope: ClosExp}
    | RAISE           of {exp: ClosExp}
    | HANDLE          of ClosExp * ClosExp
    | SWITCH_I        of {switch: IntInf.int Switch, precision: int}
    | SWITCH_W        of {switch: IntInf.int Switch, precision: int}
    | SWITCH_S        of string Switch
    | SWITCH_C        of (con*con_kind) Switch
    | SWITCH_E        of excon Switch
    | CON0            of {con: con, con_kind: con_kind, aux_regions: sma list, alloc: sma}
    | CON1            of {con: con, con_kind: con_kind, alloc: sma, arg: ClosExp}
    | DECON           of {con: con, con_kind: con_kind, con_exp: ClosExp}
    | DEREF           of {exp: ClosExp}
    | REF             of sma * ClosExp
    | ASSIGN          of sma * ClosExp * ClosExp
    | DROP            of {exp: ClosExp}
    | RESET_REGIONS   of {force: bool,
                          regions_for_resetting: sma list}
    | CCALL           of {name: string,
                          args: ClosExp list,
                          rhos_for_result : ClosExp list}
    | CCALL_AUTO      of {name: string,
                          args: (ClosExp * foreign_type) list,
                          res: foreign_type,
                          rhos_for_result : ClosExp list}   (* boxed res implies memory for the result *)
    | EXPORT          of {name: string,
                          clos_lab: label,
                          arg: ClosExp * foreign_type * foreign_type}
    | FRAME           of {declared_lvars: {lvar: lvar, label: label} list,
                          declared_excons: {excon: excon, label: label} list}

    and 'a Switch = SWITCH of ClosExp * ('a * ClosExp) list * ClosExp

    and sma =
      ATTOP_LI of ClosExp * pp
    | ATTOP_LF of ClosExp * pp
    | ATTOP_FI of ClosExp * pp
    | ATTOP_FF of ClosExp * pp
    | ATBOT_LI of ClosExp * pp
    | ATBOT_LF of ClosExp * pp
    | SAT_FI   of ClosExp * pp
    | SAT_FF   of ClosExp * pp
    | IGNORE

    datatype TopDecl =
      FUN of label * cc * ClosExp
    | FN of label * cc * ClosExp

    type ClosPrg = TopDecl list

    type env
    val empty: env
    val init: env
    val plus: env * env -> env
    val enrich : env * env -> bool
    val match : env * env -> unit
    val restrict : env * {lvars:lvar list,
                          cons:con list,
                          excons:excon list} -> env

    (* restrict0 : Don't include predeclared regions *)
    val restrict0 : env * {lvars:lvar list,
                           cons:con list,
                           excons:excon list} -> env
    val pu : env Pickle.pu

    val retrieve_lvar : env -> lvar -> label option
    val conkind : env -> con -> con_kind

    val cc : env * ((place*pp) at, place*phsize, unit)LambdaPgm -> {main_lab:label,
                                                                    code:ClosPrg,
                                                                    env:env,
                                                                    imports:label list * label list,
                                                                    exports:label list * label list}

    type StringTree
    val layout_clos_exp : ClosExp -> StringTree
    val layout_top_decl : TopDecl -> StringTree
    val layout_clos_prg : ClosPrg -> StringTree
    val layout_env      : env -> StringTree

    val pr_rhos : place list -> string
    val pr_lvars : lvar list -> string
    val pr_excons : excon list -> string
    val pr_free : lvar list * excon list * place list -> string

  end
