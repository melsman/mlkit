
signature COMPILE_GEN =
  sig

    (* Compiler for compiling structure declarations. If no code is
     * generated, only a CEnv is returned. *)

    type CompBasis
    type CEnv
    type strdec
    type funid and strid and Env and strexp
    type target
    type longid
    type lvar

    (* Hook to be run before any compilation *)
    val preHook : unit -> unit

    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook : {unitname:string} -> unit

    datatype res = CodeRes of CEnv * CompBasis * target * bool
                 | CEnvOnlyRes of CEnv      (* the boolean is true
                                             * if the code has no
                                             * side-effects (is safe) *)
    val compile :
	('a * ('a -> funid -> strid * Env * strexp * CEnv * 'a))
	-> CEnv * CompBasis * strdec list -> res

    datatype 'a cval = VAR of 'a | STR of string | UNKN
    val retrieve_longid : CEnv -> CompBasis -> longid -> lvar cval

  end
