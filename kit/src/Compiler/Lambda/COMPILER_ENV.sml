(*$COMPILER_ENV*)
(* COMPILER_ENV: used by CompileDec to look up identifiers, returning lvars
   (for things which have actually been declared) or prims (for built-in
   functions). *)

signature COMPILER_ENV =
  sig
    type CEnv
    type id				(* Identifiers. *)
    type con                            (* Unqualified value constructors. *)
    type excon				(* Unqualified exception constructors.*)
    type Type				(* Lambda Type *)
    type lvar				(* Unique lambda identifiers. *)

    type instance_transformer
    val mk_it : ''a list -> ''a list -> instance_transformer
    val apply_it : instance_transformer * 'a list -> 'a list

    val emptyCEnv: CEnv
    val initialCEnv: CEnv

    val declareVar: (id * lvar * CEnv) -> CEnv
    val declareCon: (id * (con * instance_transformer) * CEnv) -> CEnv
    val declareExcon: (id * excon * CEnv) -> CEnv

    val plus: CEnv * CEnv -> CEnv

    datatype result = 
        LVAR of lvar
      | EXCON of excon
      | CON of con * instance_transformer 
      | REF
      | ABS | NEG | PLUS | MINUS | MUL | LESS 
      | GREATER | LESSEQ | GREATEREQ
      | RESET_REGIONS | FORCE_RESET_REGIONS
      | PRIM		(* Looking up a value identifier will
			 either get you a real lvar,
			 an overloaded operator,
			 or PRIM (which means you have to
			 extract the integer argument and
			 use it in a PRIM lambda-exp). *)

    val lookupId: CEnv -> id -> result
        (* Only `prim' should give you back `PRIM' (and even then it can be 
	 * overwritten). *)

    val declareLvar : (lvar * Type list * CEnv) -> CEnv      (* these are local for each program unit *)      
    val lookupLvar : CEnv -> lvar -> Type list

    val lvarsOfCEnv: CEnv -> lvar list
      (* Return the list of lvars which the declared ids in CEnv are mapped to *)
    val primlvarsOfCEnv: CEnv -> lvar list -> lvar list
      (* Adds the list of `primitive' lvars associated with overloaded primitives in CEnv *) 
    val exconsOfCEnv: CEnv -> excon list
      (* Return the list of excons which the declared ids in CEnv are mapped to *)
    val consOfCEnv: CEnv -> con list
      (* Return the list of cons which the declared ids in CEnv are mapped to *)

    val restrictCEnv : CEnv * {ids:id list} -> CEnv
    val enrichCEnv : CEnv * CEnv -> bool

    val match : CEnv * CEnv -> unit

    type StringTree
    val layoutCEnv: CEnv -> StringTree
  end;
