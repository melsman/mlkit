(*$ElabRepository: NAME FINMAP TYNAME ELAB_REPOSITORY INFIX_BASIS*)

functor ElabRepository(structure Name : NAME
		       structure FinMap : FINMAP
		       structure TyName : TYNAME
		       structure InfixBasis : INFIX_BASIS
		       eqtype funid
		       type ElabBasis
		       type realisation
		       structure Crash : CRASH) : ELAB_REPOSITORY =
  struct

    open Edlib
    open General

    type name = Name.name
     and InfixBasis = InfixBasis.Basis
     and funid = funid
     and ElabBasis = ElabBasis
     and realisation = realisation

    structure TyName = TyName

    fun die s = Crash.impossible ("ElabRepository."^s)

    val empty_infix_basis : InfixBasis = InfixBasis.emptyB

    type prjid = string
    type elabRep = (prjid * funid, (InfixBasis * ElabBasis * (realisation * TyName.Set.Set) * name list * 
				    InfixBasis * ElabBasis * realisation) list) FinMap.map ref
    val elabRep : elabRep = ref FinMap.empty

    fun clear() = elabRep := FinMap.empty

    fun delete_rep rep prjid_and_funid = case FinMap.remove (prjid_and_funid, !rep)
					   of OK res => rep := res
					    | _ => ()
    fun delete_entries funid = delete_rep elabRep funid

    fun lookup_rep rep exportnames_from_entry prjid_and_funid =
      let val all_gen = List.foldR (fn n => fn b => b andalso
				    Name.is_gen n) true
	  fun find ([], n) = NONE
	    | find (entry::entries, n) = 
	    if (all_gen o exportnames_from_entry) entry then SOME(n,entry)
	    else find(entries,n+1)
      in case FinMap.lookup (!rep) prjid_and_funid
	   of SOME entries => find(entries, 0)
	    | NONE => NONE
      end

    fun add_rep rep (prjid_and_funid,entry) : unit =
      rep := let val r = !rep 
	     in case FinMap.lookup r prjid_and_funid
		  of SOME res => FinMap.add(prjid_and_funid,res @ [entry],r)
		   | NONE => FinMap.add(prjid_and_funid,[entry],r)
	     end

    fun owr_rep rep (prjid_and_funid,n,entry) : unit =
      rep := let val r = !rep
		 fun owr(0,entry::res,entry') = entry'::res
		   | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		   | owr _ = die "owr_rep.owr"
	     in case FinMap.lookup r prjid_and_funid
		  of SOME res => FinMap.add(prjid_and_funid,owr(n,res,entry),r)
		   | NONE => die "owr_rep.NONE"
	     end

    val lookup_elab = lookup_rep elabRep #4 
    val add_elab = add_rep elabRep
    val owr_elab = owr_rep elabRep

    fun recover() =
      List.apply 
      (List.apply (fn entry => List.apply Name.mark_gen (#4 entry)))
      (FinMap.range (!elabRep))

  end
