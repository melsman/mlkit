
signature COMPILE =
  sig

    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CEnv and CompileBasis and strdec 

    type label

    type StoreTypeCO
    type offset = int
    type AtySS
    type ('sty, 'offset, 'aty) LinePrg
    type target_new = {main_lab: label,
		       code: (StoreTypeCO,offset,AtySS) LinePrg,
		       imports: label list * label list,
		       exports: label list * label list,
		       safe: bool}     (* true if the fragment has no side-effects;
					* for dead code elimination. *)

    datatype res = CodeRes of CEnv * CompileBasis * target_new
                 | CEnvOnlyRes of CEnv

    val compile : CEnv * CompileBasis * strdec list * string -> res

  end 
