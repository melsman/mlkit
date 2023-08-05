(* Constructors for the lambda language *)

structure Con :> CON where type name = Name.name =
  struct
    structure PP = PrettyPrint

    (* Constructors are based on names which may be `matched'. In
     * particular, if two constructors, c1 and c2, are successfully
     * matched, eq(c1,c2) = true. This may affect the ordering of
     * constructors. *)

    type name = Name.name
    type con = {str: string, name: name}

    fun mk_con (s: string) : con = {str=s,name=Name.new()}

    fun pr_con ({str,...}: con) : string = str
    fun pr_con' ({str,name}: con) : string = str ^ "_" ^ Int.toString (#1(Name.key name))

    fun name ({name,...}: con) : name = name

    val op < = fn (con1, con2) =>
      let val s1 = pr_con con1
	  val s2 = pr_con con2
      in if s1 = s2 then Name.lt (name con1, name con2)
	 else s1 < s2
      end

    fun eq (con1, con2) = Name.eq (name con1, name con2)
    fun match (con1, con2) = Name.match(name con1, name con2)

    (* Predefined Constructors *)
    local
	val bucket = ref nil
	fun predef s =
	    let val c = mk_con s
	    in bucket := c :: !bucket
		; c
	    end
    in
	val con_REF   : con      = predef "ref"
	val con_TRUE  : con      = predef "true"
	val con_FALSE : con      = predef "false"
	val con_NIL   : con      = predef "nil"
	val con_CONS  : con      = predef "::"
	val con_QUOTE : con      = predef "QUOTE"
	val con_ANTIQUOTE : con  = predef "ANTIQUOTE"

	val con_INTINF : con     = predef "IntInf"

	val consPredefined = !bucket
    end

    val pu =
	Pickle.hashConsEq eq
	(Pickle.register "Con" consPredefined
	 let fun to (s,n) : con = {str=s,name=n}
	     fun from ({str=s,name=n} : con) = (s,n)
	 in Pickle.newHash (#1 o Name.key o #name)
	     (Pickle.convert (to,from) (Pickle.pairGen0(Pickle.string,Name.pu)))
	 end)

    structure QD : QUASI_DOM =
      struct
	type dom = con
	type name = Name.name
	val name = name
	val pp = pr_con
      end

    structure Map = QuasiMap(QD)

  end
