
structure InfixBasis: INFIX_BASIS =
  struct
    structure PP = PrettyPrint

    fun die s = Crash.impossible ("InfixBasis." ^ s)

    type id = Ident.id

    datatype InfixEntry = NONFIX | INFIX of int | INFIXR of int

    fun pr_InfixEntry fix =
      case fix
	of NONFIX => "nonfix"
	 | INFIX n => "infix " ^ Int.toString n
	 | INFIXR n => "infixr " ^ Int.toString n

    type Basis = InfixEntry Ident.Map.map

    val emptyB = Ident.Map.empty

   (* new - we declare identifiers in groups (since that's what the ML
	    syntax supports). Each such declaration creates a new
    	    environment which we may then add to existing environments. *)

    local
      fun declare iBas (id, fix) = Ident.Map.add(id, fix, iBas)
    in
      fun new(ids, fix) =
	foldl (fn (id,iBas) => declare iBas (id, fix)) emptyB ids
    end

    fun lookup iBas id =
      case Ident.Map.lookup iBas id
	of SOME fix => fix
	 | NONE => NONFIX

    fun compose a = Ident.Map.plus a

    fun eq (B1, B2) =
        Ident.Map.list B1 = Ident.Map.list B2

    type Report = Report.Report

    val reportBasis =
      Ident.Map.reportMap
          (fn (id, entry) =>
	      Report.line(pr_InfixEntry entry ^ " " ^ Ident.pr_id id)
	  )

    type StringTree = PP.StringTree

    val layoutBasis =
      Ident.Map.layoutMap {start="<iBas: ", eq=" -> ", sep="; ", finish=">"}
      		       (PP.layoutAtom Ident.pr_id)
		       (PP.layoutAtom pr_InfixEntry)

    val pu_InfixEntry =
	let fun toInt NONFIX = 0
	      | toInt (INFIX _) = 1
	      | toInt (INFIXR _) = 2
	    val fun_NONFIX = Pickle.con0 NONFIX
	    fun fun_INFIX _ =
		Pickle.con1 INFIX (fn INFIX a => a | _ => die "pu_InfixEntry.INFIX")
		Pickle.int
	    fun fun_INFIXR _ =
		Pickle.con1 INFIXR (fn INFIXR a => a | _ => die "pu_InfixEntry.INFIXR")
		Pickle.int
	in Pickle.dataGen("InfixEntry",toInt,[fun_NONFIX,fun_INFIX,fun_INFIXR])
	end

    val pu : Basis Pickle.pu = Ident.Map.pu Ident.pu pu_InfixEntry

  end
