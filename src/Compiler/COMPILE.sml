
signature COMPILE =
  sig

    (* Compiler for compiling structure declarations that do not contain
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CEnv and CompileBasis and strdec 

    type target (* for the old backend *)

    type linkinfo and EA
    type label

    val code_label_of_linkinfo : linkinfo -> label
    val imports_of_linkinfo : linkinfo -> label list
    val exports_of_linkinfo : linkinfo -> label list
    val unsafe_linkinfo : linkinfo -> bool
    val mk_linkinfo : {code_label:label, imports:label list,
		       exports:label list, unsafe:bool} -> linkinfo

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

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo * target_new
                 | CEnvOnlyRes of CEnv

    val compile : CEnv * CompileBasis * strdec list * string -> res

    val generate_link_code : label list -> target        (* for the old backend *)
    val emit: {target: target, filename:string} -> unit  (* for the old backend *)

  end 
