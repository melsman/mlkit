
signature REGION_EXP = sig

    (* Intermediate language used for region inference. The language
     * is typed and functions are allowed to accept and return
     * multiple arguments. This is done by allowing two kinds of
     * records: boxed and unboxed. Unboxed records are supposed to be
     * represented in registers.
     *
     * Value and exceptions constructors are supposed to be
     * distinct, which must be ensured by the compiler frontend. *)

    type lvar
    type con
    type excon
    type TyName
    type place and effect

    eqtype tyvar

    type Type and mu and sigma and il and cone

    datatype constructorKind = CONSTANT | VALUE_CARRYING
    datatype datbinds = DATBINDS of (TyName * (con * constructorKind * sigma) list) list list

    datatype metaType =
                            (* describes normal expressions: *)
        Mus of mu list
                            (* To allow the result of a declaration: *)
      | Frame of {declared_lvars: {lvar : lvar,
				   compound : bool,
				   create_region_record : bool,
                                   regvars : RegVar.regvar list,
                                   sigma: sigma ref,
                                   place: place option} list,
		  declared_excons: (excon * mu option) list}

      | RaisedExnBind (* to be a raised Bind exception. *)


    datatype ('a,'b) LambdaPgm = PGM of
           {expression: ('a,'b)trip,
            export_datbinds: datbinds,
            export_basis: effect list}

      (* list of mutual recursive datatype declarations *)

    and ('a,'b)trip = TR of ('a,'b)LambdaExp * metaType * effect
    and ('a,'b)LambdaExp =
        VAR      of {lvar: lvar, il_r : (il * (il * cone -> il * cone)) ref, fix_bound: bool}
      | INTEGER  of IntInf.int * Type * 'a option  (* NONE if unboxed *)
      | WORD     of IntInf.int * Type * 'a option  (* NONE if unboxed *)
      | STRING   of string * 'a
      | REAL     of string * 'a
      | F64      of string
      | UB_RECORD of ('a,'b) trip list (* unboxed records *)
      | FN       of {pat : (lvar * mu) list,
                     body : ('a,'b)trip,
                     alloc: 'a,
		     free: (lvar list * excon list) option}  (*region inference without dangling pointers*)
      | LETREGION_B of {B: effect list ref, discharged_phi: effect list ref, body: ('a,'b)trip}
      | LET      of {pat : (lvar * (tyvar*effect option) list * Type * place option) list,   (* memo: delete tyvar list *)
		     bind : ('a,'b)trip,
		     scope: ('a,'b)trip}
      | FIX      of {shared_clos: 'a,
                     functions : {lvar : lvar,
                                  occ: (il * (il * cone -> il * cone)) ref list ref,
				  tyvars : (tyvar*effect option) list ref,                   (* spurious tyvars are annotated with effects *)
                                  rhos: place list ref,
                                  epss: effect list ref,
				  Type : Type,
                                  formal_regions: 'b list option,
				  bind : ('a,'b)trip} list,
		     scope : ('a,'b)trip}
      | APP      of ('a,'b)trip * ('a,'b)trip
      | EXCEPTION of excon * bool * mu * 'a * ('a,'b)trip
                             (* mu: of exception constructor
                                bool: true if exception is nullary *)
      | RAISE    of ('a,'b)trip
      | HANDLE   of ('a,'b)trip * ('a,'b)trip
      | SWITCH_I of {switch: ('a,'b,IntInf.int) Switch, precision: int}
      | SWITCH_W of {switch: ('a,'b,IntInf.int) Switch, precision: int}
      | SWITCH_S of ('a,'b,string) Switch
      | SWITCH_C of ('a,'b,con) Switch
      | SWITCH_E of ('a,'b,excon) Switch
      | CON0     of {con : con, il : il, aux_regions: 'a list, alloc: 'a option}   (* NONE if unboxed *)
      | CON1     of {con : con, il : il, alloc: 'a option} * ('a,'b)trip           (* NONE if unboxed *)
      | DECON    of {con : con, il : il} * ('a,'b)trip
      | EXCON    of excon * ('a * ('a,'b)trip) option     (* nullary excons are looked up in dyn env. *)
      | DEEXCON  of excon * ('a,'b)trip
      | RECORD   of 'a option * ('a,'b)trip list
      | SELECT   of int * ('a,'b)trip
      | DEREF    of ('a,'b)trip
      | REF      of 'a * ('a,'b)trip
      | ASSIGN   of ('a,'b)trip * ('a,'b)trip
      | DROP     of ('a,'b)trip  (* to do wild cards properly; drops the type *)
      | EQUAL    of {mu_of_arg1: mu, mu_of_arg2: mu} * ('a,'b)trip * ('a,'b)trip
      | CCALL    of {name : string,
		     mu_result : mu, (*mu of result from c function*)
		     rhos_for_result : ('a * int option) list}
	            * ('a,'b)trip list  (* Calling C functions *)

      | BLOCKF64 of 'a * ('a,'b)trip list
      | SCRATCHMEM of int * 'a  (* bytes; type string *)

      (*`rhos_for_result' is technical; see comment in signature MUL_EXP*)

      | EXPORT   of {name : string,
		     mu_arg : mu, (*mu of argument to c function*)
		     mu_res : mu}
	            * ('a,'b)trip  (* The ML function *)

      | RESET_REGIONS of {force: bool, regions_for_resetting: 'a list}
                         * ('a,'b)trip     (* for programmer-directed resetting of regions;
				            * resetting is forced iff "force" is true.
				            * Forced resetting is not guaranteed to be sound *)
      | FRAME    of {declared_lvars: {lvar : lvar,
                                      regvars : RegVar.regvar list,
                                      sigma: sigma ref,
                                      place: place option} list,
                     declared_excons: (excon * mu option) list}
                       (* a frame is the result of a structure-level
                        * declaration.
			*)

    and ('a,'b,'c) Switch = SWITCH of ('a,'b)trip *
                                      ('c * ('a,'b)trip) list * ('a,'b)trip option


    val mkPhi: (place,'b)trip * effect list -> effect list

    val letregionBound : (place,'b)trip -> effect list

    val normPgm: (place, 'b)LambdaPgm * (unit -> int) -> unit

    val pr_tyvar : tyvar -> string

    type StringTree

    val layMeta : metaType -> StringTree
    val layoutLambdaPgm: ('a -> StringTree option) -> ('b -> StringTree option) ->
                         ('a, 'b)LambdaPgm -> StringTree
    val layoutLambdaExp: ('a -> StringTree option) -> ('b -> StringTree option) ->
                         ('a,'b)LambdaExp -> StringTree
    val layoutLambdaExp': (place, unit)LambdaExp ->StringTree
  end
