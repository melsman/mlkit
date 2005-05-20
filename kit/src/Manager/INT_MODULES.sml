
signature INT_MODULES =
  sig
    type IntBasis 
    type topdec = PostElabTopdecGrammar.topdec
    type modcode

    type absprjid

    val interp : bool * absprjid * IntBasis * topdec * string -> IntBasis * modcode    

    (* Can effect repository. The string is the string-rep of the
     funid for the unit. The boolean specifies whether functor
     applications in this topdec should be inlined. *)

  end
