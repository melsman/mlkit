(* Exception constructors for the lambda language *)

structure Excon: EXCON =
  struct
    structure PP = PrettyPrint

    (* Exception constructors are based on names which may be
     * `matched'. In particular, if two exception constructors, exc1
     * and exc2, are successfully matched, eq(exc1,exc2) = true. This
     * may affect the canonical ordering of exception constructors. *)

    type name = Name.name
    type excon = {str: string, name: name}

    fun mk_excon (s: string) : excon = {str=s,name=Name.new()}
    fun pr_excon ({str,...}: excon) : string = str
    fun pr_excon' ({str,name}: excon) : string = str ^ "_" ^ Int.toString (#1(Name.key name))

(*    val pr_excon = pr_excon'  (* MEMO; mael 2004-03-03 *) *)

    fun name ({name,...}: excon) : name = name

    val op < = fn (e1,e2) => Name.lt (name e1,name e2)
    fun eq (e1,e2) = Name.eq (name e1, name e2)
    fun match (excon1,excon2) = Name.match(name excon1, name excon2)

    (* Predefined exception constructors *)
    local
	val bucket = ref nil
	fun predef s =
	    let val c = mk_excon s
	    in bucket := c :: !bucket
		; c
	    end
    in
	val ex_DIV       : excon  = predef "Div"
	val ex_MATCH     : excon  = predef "Match"
	val ex_BIND      : excon  = predef "Bind"
	val ex_OVERFLOW  : excon  = predef "Overflow"
	val ex_INTERRUPT : excon  = predef "Interrupt"
	val ex_SUBSCRIPT : excon  = predef "Subscript"
	val ex_SIZE      : excon  = predef "Size"
	val exconsPredefined = !bucket
    end

    val pu =
	Pickle.hashConsEq eq
	(Pickle.register "Excon" exconsPredefined
	 let fun to (s,n) : excon = {str=s,name=n}
	     fun from ({str=s,name=n} : excon) = (s,n)
	 in Pickle.newHash (#1 o Name.key o #name)
	     (Pickle.convert (to,from) (Pickle.pairGen0(Pickle.string,Name.pu)))
	 end)

    structure QD : QUASI_DOM =
      struct
	type dom = excon
	type name = Name.name
	val name = name
	val pp = pr_excon
      end

    structure Map = QuasiMap(QD)

  end
