
signature LAMBDA_EXP =
  sig

    (* The lambda language is typed and functions are allowed to
     * accept and return multiple arguments. This is done by allowing
     * two kinds of records: boxed and unboxed. Unboxed records are
     * supposed to be represented in registers.
     *
     * Value and exceptions constructors are supposed to be
     * distinct. This must be ensured by the compiler.
     *
     * The language is supposed to be used as follows: First, the
     * high-level abstract syntax tree of the compiler is translated
     * into this language. Second, simple optimizations are performed
     * (OptLambda, e.g.). Finally, boxing analysis is performed as a
     * source-to-source transformation on the language. -- martin *)


    type lvar
    type con
    type excon
    type TyName
    type regvar
    type Report

    datatype ateff =   (* ReML atomic effect *)
        VARateff of regvar
      | PUTateff of regvar
      | GETateff of regvar

    datatype eff =     (* ReML effect *)
        SETeff of ateff list
      | VAReff of regvar

    datatype prop =
        NOMUTprop | NOPUTprop | NOEXNprop

    val pp_prop : prop -> string

    datatype constr =  (* ReML constraints *)
        DISJOINTconstr of eff * eff * bool * Report * lvar option  (* true if put-only *)
      | INCLconstr of regvar * eff * Report * lvar option
      | PROPconstr of prop * eff * Report * lvar option

    eqtype tyvar
    val fresh_tyvar : unit -> tyvar
    val fresh_eqtyvar : unit -> tyvar
    val pr_tyvar : tyvar -> string
    val lt_tyvar : tyvar * tyvar -> bool
    val equality_tyvar : tyvar -> bool
    val reset : unit -> unit

    datatype Type =
        TYVARtype   of tyvar
      | ARROWtype   of Type list * regvar option * Type list * regvar option
      | CONStype    of Type list * TyName * regvar list option
      | RECORDtype  of Type list * regvar option

    val tyvars : Type -> tyvar list  (* without duplicates *)

    (* word8 is compiled into default word-type in CompileDec *)
    val boolType: Type
    val unitType: Type
    val foreignptrType: Type
    val exnType : Type
    val int31Type : Type
    val int32Type : Type
    val int63Type : Type
    val int64Type : Type
    val intinfType : Type
    val intDefaultType : unit -> Type   (* int63 if tag_values, otherwise int64 *)
    val word31Type : Type
    val word32Type : Type
    val word63Type : Type
    val word64Type : Type
    val wordDefaultType : unit -> Type  (* word63 if tag_values, otherwise word64 *)
    val realType: Type
    val f64Type: Type
    val stringType: Type
    val chararrayType: Type

    datatype TypeList =                               (* To allow the result of a declaration *)
        Types of Type list                            (* to be a raised Bind exception. *)
      | Frame of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
                  declared_excons: (excon * Type option) list}
      | RaisedExnBind

    datatype 'Type prim =                             (* The primitives are always fully applied ! *)
        CONprim of {con : con, instances : 'Type list, regvar: regvar option}
      | DECONprim of {con : con, instances : 'Type list, lv_opt: lvar option}
      | EXCONprim of excon
      | DEEXCONprim of excon
      | RECORDprim of regvar option
      | SELECTprim of int
      | UB_RECORDprim                                 (* Unboxed record. *)
      | DROPprim
      | DEREFprim of {instance: 'Type}
      | REFprim of {instance: 'Type, regvar: regvar option}
      | ASSIGNprim of {instance: 'Type}
      | EQUALprim of {instance: 'Type}
      | CCALLprim of {name : string,                  (* Primitives, etc. *)
                      instances : 'Type list,
                      tyvars : tyvar list,
                      Type : 'Type}
      | BLOCKF64prim
      | SCRATCHMEMprim of int                         (* bytes *)
      | EXPORTprim of {name : string,
                       instance_arg : 'Type,
                       instance_res : 'Type}
      | RESET_REGIONSprim of {instance: 'Type}        (* NOT Standard ML, for programmer-directed,
                                                          but safe, resetting of regions *)
      | FORCE_RESET_REGIONSprim of {instance: 'Type}  (* NOT Standard ML, for programmer-controlled,
                                                          unsafe resetting of regions *)

    datatype LambdaPgm = PGM of datbinds * LambdaExp

    and datbinds = DATBINDS of (tyvar list * TyName * (con * Type option) list) list list
      (* list of mutual recursive datatype declarations *)

    and LambdaExp =
        VAR      of {lvar: lvar, instances : Type list, regvars: regvar list}
      | INTEGER  of IntInf.int * Type
      | WORD     of IntInf.int * Type
      | STRING   of string * regvar option
      | REAL     of string * regvar option
      | F64      of string
      | FN       of {pat : (lvar * Type) list, body : LambdaExp}
      | LET      of {pat : (lvar * tyvar list * Type) list,
                     bind : LambdaExp,
                     scope: LambdaExp}
      | LETREGION of {regvars: regvar list,
                      scope: LambdaExp}
      | FIX      of {functions : {lvar : lvar,
                                  regvars: regvar list,
                                  tyvars : tyvar list,
                                  Type : Type,
                                  constrs: constr list,
                                  bind : LambdaExp} list,
                     scope : LambdaExp}
      | APP      of LambdaExp * LambdaExp * bool option  (* tail call? *)
      | EXCEPTION of excon * Type option * LambdaExp
      | RAISE    of LambdaExp * TypeList
      | HANDLE   of LambdaExp * LambdaExp
      | SWITCH_I of {switch: IntInf.int Switch, precision: int}
      | SWITCH_W of {switch: IntInf.int Switch, precision: int}
      | SWITCH_S of string Switch
      | SWITCH_C of (con*lvar option) Switch
      | SWITCH_E of (excon*lvar option) Switch
      | TYPED    of LambdaExp * Type * constr list
      | PRIM     of Type prim * LambdaExp list
      | FRAME    of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
                     declared_excons: (excon * Type option) list}
                       (* a frame is the result of a structure-level
                        * declaration.
                        *)

    and 'a Switch = SWITCH of LambdaExp * ('a * LambdaExp) list * LambdaExp option

    val size: LambdaExp -> int
    val size_incl_types: LambdaExp -> int

    (* These predicates approximate whether a lambda program performs
     * side effects; they are used to determine if a program unit can
     * be discharged at link time in case it is not used. They are also
     * used by the optimiser to remove bindings of variables that are
     * not used. *)
    val safeLambdaExp: LambdaExp -> bool
    val safeLambdaExps: LambdaExp list -> bool
    val safeLambdaPgm: LambdaPgm -> bool

    type StringTree
    val layoutLambdaPgm: LambdaPgm -> StringTree
    val layoutLambdaExp: LambdaExp -> StringTree
    val layoutType     : Type -> StringTree
    val layoutTypeScheme : tyvar list * Type -> StringTree
    val layoutTypeList : TypeList -> StringTree
    val layoutPrim     : ('Type -> StringTree) -> 'Type prim -> StringTree

    (* Generate ML code *)
    val barify : LambdaPgm -> StringTree

    val pu_intinf     : IntInf.int Pickle.pu
    val pu_tyvar      : tyvar Pickle.pu
    val pu_tyvars     : tyvar list Pickle.pu
    val pu_Type       : Type Pickle.pu
    val pu_Types      : Type list Pickle.pu
    val pu_TypeScheme : (tyvar list * Type) Pickle.pu
    val pu_LambdaExp  : LambdaExp Pickle.pu
    val pu_prop       : prop Pickle.pu

    structure TyvarSet : KIT_MONO_SET where type elt = tyvar
    val tyvars_Exp    : TyvarSet.Set -> LambdaExp -> TyvarSet.Set -> TyvarSet.Set
    val tyvars_Type   : TyvarSet.Set -> Type -> TyvarSet.Set -> TyvarSet.Set
    val tyvars_TypeList : TyvarSet.Set -> TypeList -> TyvarSet.Set -> TyvarSet.Set
    val tyvars_Prim   : TyvarSet.Set -> Type prim -> TyvarSet.Set -> TyvarSet.Set

    structure TyvarMap : MONO_FINMAP where type dom = tyvar
  end
