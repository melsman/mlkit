(* Constructors for the lambda language *)

functor Con(structure Name : NAME
	    structure Report : REPORT
	    structure Crash : CRASH
	    structure PP : PRETTYPRINT
	    structure IntStringFinMap : MONO_FINMAP where type dom = int * string
	      ) : CON =
  struct

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
    val con_REF   : con  = mk_con "ref"
    val con_TRUE  : con  = mk_con "true"
    val con_FALSE : con  = mk_con "false"
    val con_NIL   : con  = mk_con "nil"
    val con_CONS  : con  = mk_con "::"

    val con_QUOTE : con  = mk_con "QUOTE"
    val con_ANTIQUOTE : con  = mk_con "ANTIQUOTE"

    val pu = 
	Pickle.hashConsEq eq
	(Pickle.register "Con" [con_REF,con_TRUE,con_FALSE,con_NIL,con_CONS,
				con_QUOTE,con_ANTIQUOTE]
	 let open Pickle
	     fun to (s,n) : con = {str=s,name=n}
	     fun from ({str=s,name=n} : con) = (s,n)
	 in newHash (#1 o Name.key o #name)
	     (convert (to,from) (pairGen0(string,Name.pu)))
	 end)

    structure QD : QUASI_DOM =
      struct
	type dom = con
	type name = Name.name
	val name = name
	val pp = pr_con
      end

    structure Map = QuasiMap(structure IntStringFinMap = IntStringFinMap
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PP
			     structure Report = Report
			     structure QD = QD)

  end