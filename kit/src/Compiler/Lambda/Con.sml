(* Constructors for the lambda language *)

(*$Con: NAME CON *)

functor Con(structure Name : NAME) : CON =
  struct

    (* Constructors are based on names which may be `matched'. In
     * particular, if two constructors, c1 and c2, are successfully
     * matched, eq(c1,c2) = true. This may affect the ordering of 
     * constructors. *)

    type name = Name.name
    type con = {str: string, name: name}

    fun mk_con (s: string) : con = {str=s,name=Name.new()}

    fun pr_con ({str,...}: con) : string = str

    fun name ({name,...}: con) : name = name

    val op < = fn (con1, con2) => 
      let val s1 = pr_con con1
	  val s2 = pr_con con2
      in if s1 = s2 then Name.key (name con1) < Name.key (name con2)
	 else s1 < s2
      end

    fun eq (con1, con2) = Name.key (name con1) = Name.key (name con2)
    fun match (con1, con2) = Name.match(name con1, name con2)

    (* Predefined Constructors *)
    val con_REF   : con  = mk_con "ref"
    val con_TRUE  : con  = mk_con "true"
    val con_FALSE : con  = mk_con "false"
    val con_NIL   : con  = mk_con "nil"
    val con_CONS  : con  = mk_con "::"

  end