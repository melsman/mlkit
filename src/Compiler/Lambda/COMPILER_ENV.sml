(* COMPILER_ENV: used by CompileDec to look up identifiers, returning lvars
   (for things which have actually been declared) or prims (for built-in
   functions). *)

signature COMPILER_ENV =
  sig
    type CEnv
    type id and longid                  (* identifiers *)
    type tycon and longtycon            (* type constructors *)
    type strid and longstrid            (* structure identifiers *)
    type con                            (* unqualified value constructors *)
    type excon				(* unqualified exception constructors *)
    type Type				(* lambda Type *)
    type tyvar                          (* lambda tyvar *)
    type lvar				(* unique lambda identifiers *)
    type TyName

    val emptyCEnv: CEnv
    val initialCEnv: CEnv

    (* We support lazy instantiation of values and value
     * constructors. This is all dealt with in the translation
     * environment and when a value identifier is looked up in the
     * environment. No code is generated for signature constraints.
     *
     * At declaration time a value identifier is declared with a
     * lambda variable and a lambda type scheme. Implicitly, an
     * instance list consisting of the bound type variables is
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
     * variable.   -- Martin *)

    val declareVar: (id * (lvar * tyvar list * Type) * CEnv) -> CEnv
    val declareCon: (id * (con * tyvar list * Type) * CEnv) -> CEnv
    val declareExcon: (id * (excon * Type) * CEnv) -> CEnv
    val declare_strid: strid * CEnv * CEnv -> CEnv
    val declare_tycon: tycon * (TyName list * CEnv) * CEnv -> CEnv  (* a tycon is mapped to an environment 
								     * holding bindings for value 
								     * constructors. *)
    val plus: CEnv * CEnv -> CEnv         

    datatype result = 
        LVAR of lvar * tyvar list * Type * Type list
      | CON of con * tyvar list * Type * Type list
      | EXCON of excon * Type
      | REF
      | ABS | NEG | PLUS | MINUS | MUL | DIV | MOD | LESS 
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

    val lookup_longid : CEnv -> longid -> result option
    val lookup_strid : CEnv -> strid -> CEnv
    val lookup_longstrid : CEnv -> longstrid -> CEnv
    val lookup_longtycon : CEnv -> longtycon -> TyName list * CEnv   (* The resulting cenv holds bindings 
								      * for value constructors; the tyname
								      * list contains the tynames of the
								      * associated tystr. *)
    type ElabEnv and TypeScheme
    val constrain : CEnv * ElabEnv -> CEnv
 
    (* compileTypeScheme is used by constrain *)
    val set_compileTypeScheme : (TypeScheme -> tyvar list * Type) -> unit   (* MEGA HACK *)
                                                                            (* We should clean this *) 
                                                                            (* up at some point!! - Martin *)
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

    val restrictCEnv : CEnv * {longstrids: longstrid list, longvids: longid list, 
			       longtycons: longtycon list} -> CEnv
    val enrichCEnv : CEnv * CEnv -> bool

    val match : CEnv * CEnv -> unit

    type StringTree
    val layoutCEnv: CEnv -> StringTree
  end;
