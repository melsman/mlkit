
signature COMPILE =
  sig

    (* compiler for compiling structure declarations not containing
     * functor applications. If no code is generated, only a CEnv is
     * returned. *)

    type CEnv and CompileBasis and strdec and target and linkinfo and EA
    val code_label_of_linkinfo : linkinfo -> EA
    val imports_of_linkinfo : linkinfo -> EA list
    val exports_of_linkinfo : linkinfo -> EA list
    val unsafe_linkinfo : linkinfo -> bool
    val pp_EA : EA -> string

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    val compile : CEnv * CompileBasis * strdec list * string -> res

    val generate_link_code : EA list -> target
    val emit: {target: target, filename:string} -> unit

  end 
