(* Exception constructors for the lambda language *)

functor Excon(structure Name : NAME
	      structure Report : REPORT
	      structure Crash : CRASH
	      structure PP : PRETTYPRINT
	      structure IntFinMap : MONO_FINMAP where type dom = int
		) : EXCON =
  struct

    (* Exception constructors are based on names which may be
     * `matched'. In particular, if two exception constructors, exc1
     * and exc2, are successfully matched, eq(exc1,exc2) = true. This
     * may affect the canonical ordering of exception constructors. *)

    type name = Name.name
    type excon = {str: string, name: name}

    fun mk_excon (s: string) : excon = {str=s,name=Name.new()}
    fun pr_excon ({str,...}: excon) : string = str
    fun pr_excon' ({str,name}: excon) : string = str ^ "_" ^ Int.toString (Name.key name)

    fun name ({name,...}: excon) : name = name

    val op < = fn (excon1,excon2) => (Name.key (name excon1) < Name.key (name excon2))
    fun eq (excon1,excon2) = (Name.key (name excon1)) = (Name.key (name excon2))
    fun match (excon1,excon2) = Name.match(name excon1, name excon2) 

    (* Predefined exception constructors *)
    val ex_DIV   : excon  = mk_excon "Div"
    val ex_MATCH : excon  = mk_excon "Match"
    val ex_BIND  : excon  = mk_excon "Bind"
    val ex_OVERFLOW  : excon  = mk_excon "Overflow"
    val ex_INTERRUPT : excon  = mk_excon "Interrupt"

    val pu = 
	Pickle.register [ex_DIV, ex_MATCH, ex_BIND, 
			 ex_OVERFLOW, ex_INTERRUPT]
	let open Pickle
	    fun to (s,n) : excon = {str=s,name=n}
	    fun from ({str=s,name=n} : excon) = (s,n)
	in convert (to,from) (pairGen(string,Name.pu))
	end
	
    structure QD : QUASI_DOM =
      struct
	type dom = excon
	type name = Name.name
	val name = name
	val pp = pr_excon
      end

    structure Map = QuasiMap(structure IntFinMap = IntFinMap
			     structure Name = Name
			     structure Crash = Crash
			     structure PP = PP
			     structure Report = Report
			     structure QD = QD)

  end
