(*$COMPILE*)

signature COMPILE =
  sig

    type CompileBasis and topdec and target and linkinfo and name

    val generative_names : name list ref

    val compile : CompileBasis * topdec * string -> (CompileBasis * target * linkinfo) Option

    val generate_link_code : linkinfo list -> target

    val emit: {target: target, filename:string} -> unit

    val reset : unit -> unit
    val commit : unit -> unit

  end 
