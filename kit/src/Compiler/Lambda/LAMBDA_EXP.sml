
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

    eqtype tyvar 
    val fresh_tyvar : unit -> tyvar
    val fresh_eqtyvar : unit -> tyvar
    val pr_tyvar : tyvar -> string
    val lt_tyvar : tyvar * tyvar -> bool
    val equality_tyvar : tyvar -> bool
    val reset : unit -> unit
    val commit : unit -> unit

    datatype Type =
        TYVARtype   of tyvar
      | ARROWtype   of Type list * Type list
      | CONStype    of Type list * TyName
      | RECORDtype  of Type list

    val tyvars : Type -> tyvar EqSet.Set

    (* word8 is compiled into default word-type in CompileDec *)
    val boolType: Type
    val unitType: Type
    val foreignptrType: Type
    val exnType : Type
    val int31Type : Type
    val int32Type : Type
    val intDefaultType : unit -> Type   (* int31 if tag_values, otherwise int32 *)
    val word31Type : Type
    val word32Type : Type
    val wordDefaultType : unit -> Type  (* word31 if tag_values, otherwise word32 *)
    val realType: Type
    val stringType: Type

    datatype TypeList =                               (* To allow the result of a declaration *)  
        Types of Type list                            (* to be a raised Bind exception. *)
      | Frame of {declared_lvars: {lvar : lvar, tyvars: tyvar list, Type: Type} list,
		  declared_excons: (excon * Type option) list}
      | RaisedExnBind

    datatype 'Type prim =                             (* The primitives are always fully applied ! *)
        CONprim of {con : con, instances : 'Type list}
      | DECONprim of {con : con, instances : 'Type list}
      | EXCONprim of excon
      | DEEXCONprim of excon
      | RECORDprim 
      | SELECTprim of int        
      | UB_RECORDprim                                 (* Unboxed record. *)
      | DROPprim
      | DEREFprim of {instance: 'Type}
      | REFprim of {instance: 'Type}
      | ASSIGNprim of {instance: 'Type}
      | EQUALprim of {instance: 'Type}
      | CCALLprim of {name : string,                  (* Primitives, etc. *)
		      instances : 'Type list,
		      tyvars : tyvar list,
		      Type : 'Type} 
      | RESET_REGIONSprim of {instance: 'Type}        (* NOT Standard ML, for programmer-directed,
						          but safe, resetting of regions *)
      | FORCE_RESET_REGIONSprim of {instance: 'Type}  (* NOT Standard ML, for programmer-controlled,
						          unsafe resetting of regions *)

    datatype LambdaPgm = PGM of datbinds * LambdaExp

    and datbinds = DATBINDS of (tyvar list * TyName * (con * Type option) list) list list
      (* list of mutual recursive datatype declarations *)

    and LambdaExp =
        VAR      of {lvar: lvar, instances : Type list}
      | INTEGER  of Int32.int * Type
      | WORD     of Word32.word * Type
      | STRING   of string
      | REAL     of string
      | FN       of {pat : (lvar * Type) list, body : LambdaExp}
      | LET      of {pat : (lvar * tyvar list * Type) list,
		     bind : LambdaExp,
		     scope: LambdaExp}
      | FIX      of {functions : {lvar : lvar, 
				  tyvars : tyvar list,
				  Type : Type,
				  bind : LambdaExp} list,
		     scope : LambdaExp}
      | APP      of LambdaExp * LambdaExp
      | EXCEPTION of excon * Type option * LambdaExp
      | RAISE    of LambdaExp * TypeList
      | HANDLE   of LambdaExp * LambdaExp
      | SWITCH_I of {switch: Int32.int Switch, precision: int}
      | SWITCH_W of {switch: Word32.word Switch, precision: int}
      | SWITCH_S of string Switch
      | SWITCH_C of con Switch
      | SWITCH_E of excon Switch
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
  end
