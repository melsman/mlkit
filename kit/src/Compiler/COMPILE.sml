      
signature COMPILE =
  sig

    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CompBasis
    type CEnv 
    type strdec

    type place 
    type pp
    type 'a at
    type phsize
    type ('a,'b,'c) LambdaPgm

    datatype res = CodeRes of CEnv * CompBasis * ((place*pp)at,place*phsize,unit) LambdaPgm * bool
                 | CEnvOnlyRes of CEnv      (* the boolean is true (safe) if the code has no side-effects *)

    val compile : CEnv * CompBasis * strdec list -> res
  end 

