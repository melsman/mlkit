
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

    type Basis = (id, InfixEntry) FinMap.map

    val emptyB = FinMap.empty

   (* new - we declare identifiers in groups (since that's what the ML
	    syntax supports). Each such declaration creates a new
    	    environment which we may then add to existing environments. *)

    local
      fun declare iBas (id, fix) = FinMap.add(id, fix, iBas)
    in
      fun new(ids, fix) =
	foldl (fn (id,iBas) => declare iBas (id, fix)) emptyB ids
    end

    fun lookup iBas id =
      case FinMap.lookup iBas id
	of SOME fix => fix
	 | NONE => NONFIX

    fun compose a = FinMap.plus a

    fun eq (B1, B2) =
      let fun sorter (a,_) (b,_) = Ident.< (a,b)
	  val l1 = ListSort.sort sorter (FinMap.list B1)
	  val l2 = ListSort.sort sorter (FinMap.list B2)
      in l1 = l2
      end

    type Report = Report.Report

    val reportBasis =
      FinMap.reportMapSORTED
        (Ident.<) (fn (id, entry) =>
		     Report.line(pr_InfixEntry entry ^ " " ^ Ident.pr_id id)
		  )

    type StringTree = PP.StringTree

    val layoutBasis =
      FinMap.layoutMap {start="<iBas: ", eq=" -> ", sep="; ", finish=">"}
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

    val pu : Basis Pickle.pu = FinMap.pu(Ident.pu,pu_InfixEntry)

  end
