(*$COMPILER_ENV*)
(* COMPILER_ENV: used by CompileDec to look up identifiers, returning lvars
   (for things which have actually been declared) or prims (for built-in
   functions). *)

signature COMPILER_ENV =
  sig
    type CEnv
    type id				(* Identifiers. *)
    type longid                         (* Long identifiers *)
    type con                            (* Unqualified value constructors. *)
    type excon				(* Unqualified exception constructors.*)
    type Type				(* Lambda Type *)
    type tyvar                          (* Lambda tyvar *)
    type lvar				(* Unique lambda identifiers. *)
    type TyName
    type strid

    (* Instance transformers are necessary, since bound type variables
     * of datatype bindings are shared in the lambda language
     * representation and *not* shared in type environments of
     * elaboration. Instance transformers then make sure that
     * instances annotated at uses of value constructors correspond to
     * how value constructors are declared in DATATYPES in
     * LambdaExp. -- Martin *)

    type instance_transformer
    val mk_it : ''a list -> ''a list -> instance_transformer
    val apply_it : instance_transformer * 'a list -> 'a list

    val emptyCEnv: CEnv
    val initialCEnv: CEnv

    (* We support lazy instantiation of values and value
     * constructors. This is all delt with in the translation
     * environment and when a value identifier is looked up in the
     * environment. No code is generated for signature constraints.
     *
     * At declaration time a value identifier is declared with a
     * lambda variable and a lambda type scheme. Implicitly, an
     * instance list consisting of the bound type vaiables is
     * installed in the environments, also.
     *
     * Then, when constraining a translation environment to an
     * elaboration environment (signature) both value and structure
     * components may be dropped and value components may be made less
     * polymorphic, yielding a less polymorphic type scheme and
     * partially instantiation of the instance list for the value
     * component. When a value identifier is looked up in the
     * translation environment a lambda variable (or value
     * constructor) is returned together with a type scheme and a
     * (partially instantiated) instance list. An instance list for
     * the bound type variables of the type scheme is annotated (as
     * TypeInfo) in the AST at the point of the lookup. This instance
     * list together with the bound type variables form a substitution
     * which can be applied to the instance list returned by the
     * lookup, and thereby forming the instance list for the lambda
     * variable. For constructors, the instance transformer must then
     * be applied to form the final instance list.  -- Martin
    *)

    val declareVar: (id * (lvar * tyvar list * Type) * CEnv) -> CEnv
    val declareCon: (id * (con * tyvar list * Type * instance_transformer) * CEnv) -> CEnv
    val declareExcon: (id * (excon * Type) * CEnv) -> CEnv
    val declare_strid: strid * CEnv * CEnv -> CEnv

    val plus: CEnv * CEnv -> CEnv

    datatype result = 
        LVAR of lvar * tyvar list * Type * Type list
      | CON of con * tyvar list * Type * Type list * instance_transformer 
      | EXCON of excon * Type
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

        (* Only `prim' should give you back `PRIM' (and even then it can be 
	 * overwritten). *)

    val lookup_longid : CEnv -> longid -> result
    val lookup_strid : CEnv -> strid -> CEnv

    type subst
    val mk_subst : tyvar list * Type list -> subst
    val on_il : subst * Type list -> Type list

    type ElabEnv and TypeScheme
    val constrain : CEnv * ElabEnv -> CEnv
 
    (* compileTypeScheme is used by constrain *)
    val set_compileTypeScheme : (TypeScheme -> tyvar list * Type) -> unit   (* MEGA HACK *)
                                                                            (* We should clean this *) 
                                                                            (* up at some point!! - Martin *)

    val declareLvar : (lvar * Type list * CEnv) -> CEnv      (* these are local for each program unit *)
    val lookupLvar : CEnv -> lvar -> Type list               (* and should *not* be here...           *)

    val tynamesOfCEnv: CEnv -> TyName list
      (* Return the list of tynames occurring in CEnv *)
    val lvarsOfCEnv: CEnv -> lvar list
      (* Return the list of lvars which the declared ids in CEnv are mapped to *)
    val primlvarsOfCEnv: CEnv -> lvar list
      (* Adds the list of `primitive' lvars associated with overloaded primitives in CEnv *) 
    val exconsOfCEnv: CEnv -> excon list
      (* Return the list of excons which the declared ids in CEnv are mapped to *)
    val consOfCEnv: CEnv -> con list
      (* Return the list of cons which the declared ids in CEnv are mapped to *)

    val restrictCEnv : CEnv * strid list * id list -> CEnv
    val enrichCEnv : CEnv * CEnv -> bool

    val match : CEnv * CEnv -> unit

    type StringTree
    val layoutCEnv: CEnv -> StringTree
  end;
