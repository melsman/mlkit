(*$InfixBasis: IDENT FINMAP REPORT PRETTYPRINT INFIX_BASIS*)
functor InfixBasis(structure Ident: IDENT
		   structure FinMap: FINMAP

		   structure Report: REPORT
		     sharing type FinMap.Report = Report.Report

		   structure PP: PRETTYPRINT
		     sharing type FinMap.StringTree = PP.StringTree
		  ): INFIX_BASIS =
  struct
    type id = Ident.id

    datatype InfixEntry = NONFIX | INFIX of int | INFIXR of int

    fun pr_InfixEntry fix =
      case fix
	of NONFIX => "nonfix"
	 | INFIX n => "infix " ^ Int.string n
	 | INFIXR n => "infixr " ^ Int.string n

    type Basis = (id, InfixEntry) FinMap.map

    val emptyB = FinMap.empty

   (* new - we declare identifiers in groups (since that's what the ML
	    syntax supports). Each such declaration creates a new
    	    environment which we may then add to existing environments. *)

    local
      fun declare iBas (id, fix) = FinMap.add(id, fix, iBas)
    in
      fun new(ids, fix) =
	List.foldL (fn id => fn iBas => declare iBas (id, fix)) emptyB ids
    end

    fun lookup iBas id =
      case FinMap.lookup iBas id
	of Some fix => fix
	 | None => NONFIX

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
  end;
