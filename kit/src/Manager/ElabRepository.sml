(*$ElabRepository: NAME FINMAP ELAB_REPOSITORY INFIX_BASIS*)

functor ElabRepository(structure Name : NAME
		       structure FinMap : FINMAP
		       structure InfixBasis : INFIX_BASIS
		       eqtype funid
		       type ElabBasis
		       structure Crash : CRASH) : ELAB_REPOSITORY =
  struct
    type name = Name.name
     and InfixBasis = InfixBasis.Basis
     and funid = funid
     and ElabBasis = ElabBasis

    fun die s = Crash.impossible ("ElabRepository."^s)

    val empty_infix_basis : InfixBasis = InfixBasis.emptyB

    type elabRep = (funid, (InfixBasis * ElabBasis * name list * InfixBasis * ElabBasis) list) FinMap.map ref
    val elabRep : elabRep = ref FinMap.empty

    fun clear() = elabRep := FinMap.empty

    fun delete_rep rep funid = case FinMap.remove (funid, !rep)
				 of OK res => rep := res
				  | _ => ()
    fun delete_entries funid = delete_rep elabRep funid

    fun lookup_rep rep exportnames_from_entry funid =
      let val all_gen = List.foldR (fn n => fn b => b andalso
				    Name.is_gen n) true
	  fun find ([], n) = None
	    | find (entry::entries, n) = 
	    if (all_gen o exportnames_from_entry) entry then Some(n,entry)
	    else find(entries,n+1)
      in case FinMap.lookup (!rep) funid
	   of Some entries => find(entries, 0)
	    | None => None
      end

    fun add_rep rep (funid,entry) : unit =
      rep := let val r = !rep 
	     in case FinMap.lookup r funid
		  of Some res => FinMap.add(funid,res @ [entry],r)
		   | None => FinMap.add(funid,[entry],r)
	     end

    fun owr_rep rep (funid,n,entry) : unit =
      rep := let val r = !rep
		 fun owr(0,entry::res,entry') = entry'::res
		   | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		   | owr _ = die "owr_rep.owr"
	     in case FinMap.lookup r funid
		  of Some res => FinMap.add(funid,owr(n,res,entry),r)
		   | None => die "owr_rep.None"
	     end

    val lookup_elab = lookup_rep elabRep #3 
    val add_elab = add_rep elabRep
    val owr_elab = owr_rep elabRep

    fun recover() =
      List.apply 
      (List.apply (fn entry => List.apply Name.mark_gen (#3 entry)))
      (FinMap.range (!elabRep))

  end
