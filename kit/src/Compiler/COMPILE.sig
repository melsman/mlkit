      
signature COMPILE =
  sig
    
    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type place 
    type pp
    type 'a at
    type phsize
    type ('a,'b,'c) LambdaPgm
    include COMPILE_GEN 
    where type target = ((place*pp)at,place*phsize,unit) LambdaPgm

  end 

