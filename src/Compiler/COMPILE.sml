      
signature COMPILE =
  sig

    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CompBasis
    type CEnv 
    type strdec
    type funid and strid and Env and strexp
    type place 
    type pp
    type 'a at
    type phsize
    type ('a,'b,'c) LambdaPgm

    (* Hook to be run before any compilation *)
    val preHook : unit -> unit
	
    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook : {unitname:string} -> unit

    datatype res = CodeRes of CEnv * CompBasis * ((place*pp)at,place*phsize,unit) LambdaPgm * bool
                 | CEnvOnlyRes of CEnv      (* the boolean is true (safe) if the code has no side-effects *)

    val compile : 
	('a * ('a -> funid -> strid * Env * strexp * CEnv * 'a))
	-> CEnv * CompBasis * strdec list -> res
  end 

