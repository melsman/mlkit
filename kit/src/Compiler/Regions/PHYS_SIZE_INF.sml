(* Physical Size Inference *)

signature PHYS_SIZE_INF =  
  sig
        type ('a,'b,'c)LambdaPgm
	 and place 
         and 'a at
         and mul 
	type pp = int
	type lvar

        datatype phsize = INF | WORDS of int

        type env
        val empty : env
        val init : env
        val plus : env * env -> env
	val enrich : env * env -> bool
	val restrict : env * lvar list -> env

        val psi: (unit -> pp) * env * (place at, place*mul, unit)LambdaPgm ->    (*************************)
	  ((place*pp)at, place*phsize, unit)LambdaPgm * env                      (* In the resulting trip *)
		                                                                 (* free variables has    *)
		                                                                 (* been inserted.        *)
                                                                                 (*************************)
        (* appConvert(pgm) rewrites pgm, categorizing every function call
           in pgm as one of the following
 
           * tail call     to fix-bound function                (jmp)
           * non-tail call to fix-bound function                (funcall)
           * tail call     to function which is not fix-bound   (fnjmp)
           * non-tail call to function which is not fix-bound   (fncall)
        *)

        val appConvert  : ((place*pp) at, place*phsize, unit)LambdaPgm ->
                           ((place*pp) at, place*phsize, unit)LambdaPgm

        type StringTree                                                          
        val layout_env  : env -> StringTree
        val layout_pgm  : ((place*pp)at, place*phsize, unit)LambdaPgm -> StringTree

	val pu_env      : env Pickle.pu
	val pu_phsize   : phsize Pickle.pu
  end      

