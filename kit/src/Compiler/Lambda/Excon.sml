(* Exception constructors for the lambda language *)

(*$Excon: NAME EXCON *)

functor Excon(structure Name : NAME) : EXCON =
  struct

    (* Exception constructors are based on names which may be
     * `matched'. In particular, if two exception constructors, exc1
     * and exc2, are successfully matched, eq(exc1,exc2) = true. This
     * may affect the canonical ordering of exception constructors. *)

    type name = Name.name
    type excon = {str: string, name: name}

    fun mk_excon (s: string) : excon = {str=s,name=Name.new()}
    fun pr_excon ({str,...}: excon) : string = str

    fun name ({name,...}: excon) : name = name

    val op < = fn (excon1,excon2) => (Name.key (name excon1) < Name.key (name excon2))
    fun eq (excon1,excon2) = (Name.key (name excon1)) = (Name.key (name excon2))
    fun match (excon1,excon2) = Name.match(name excon1, name excon2) 

    (* Predefined exception constructors *)
    val ex_ABS   : excon  = mk_excon "Abs" 
    val ex_NEG   : excon  = mk_excon "Neg"
    val ex_SUM   : excon  = mk_excon "Sum"
    val ex_DIFF  : excon  = mk_excon "Diff"
    val ex_PROD  : excon  = mk_excon "Prod"
    val ex_DIV   : excon  = mk_excon "Div"
    val ex_MOD   : excon  = mk_excon "Mod"
    val ex_MATCH : excon  = mk_excon "Match"
    val ex_BIND  : excon  = mk_excon "Bind"

  end
