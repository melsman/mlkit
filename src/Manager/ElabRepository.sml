
functor ElabRepository(structure Name : NAME
		       structure TyName : TYNAME
		       structure InfixBasis : INFIX_BASIS
		       structure OpacityEnv : OPACITY_ENV
		       eqtype funid
                       eqtype absprjid
		       structure RepositoryFinMap : MONO_FINMAP
			 where type dom = absprjid * funid
		       structure Flags : FLAGS
		       type ElabBasis
		       type longstrid
		       val strip_install_dir' : absprjid * funid -> absprjid * funid
		       structure Crash : CRASH) : ELAB_REPOSITORY =
  struct

    structure TyName = TyName
    structure RM = RepositoryFinMap

    type name = Name.name
     and InfixBasis = InfixBasis.Basis
     and funid = funid
     and ElabBasis = ElabBasis
     and opaq_env = OpacityEnv.opaq_env
     and longstrid = longstrid

    fun die s = Crash.impossible ("ElabRepository."^s)

    val empty_infix_basis : InfixBasis = InfixBasis.emptyB
    val empty_opaq_env : opaq_env = OpacityEnv.empty

    type absprjid = absprjid
    type elab_entry = (InfixBasis * ElabBasis * longstrid list * (opaq_env * TyName.Set.Set) * 
		       name list * InfixBasis * ElabBasis * opaq_env)
    type elabRep = elab_entry list RM.map ref

    val elabRep : elabRep = ref RM.empty

    fun clear() = elabRep := RM.empty

    fun delete_rep rep absprjid_and_funid = case RM.remove (absprjid_and_funid, !rep)
					      of SOME res => rep := res
					       | _ => ()

    fun delete_entries absprjid_and_funid = delete_rep elabRep (strip_install_dir' absprjid_and_funid)

    fun lookup_rep rep exportnames_from_entry absprjid_and_funid =
      let val all_gen = foldr (fn (n, b) => b andalso
			       Name.is_gen n) true
	  fun find ([], n) = NONE
	    | find (entry::entries, n) = 
	    if (all_gen o exportnames_from_entry) entry then SOME(n,entry)
	    else find(entries,n+1)
      in case RM.lookup (!rep) (strip_install_dir' absprjid_and_funid)
	   of SOME entries => find(entries, 0)
	    | NONE => NONE
      end

    fun add_rep rep (absprjid_and_funid,entry) : unit =
      rep := let val r = !rep 
                 val i = strip_install_dir' absprjid_and_funid
	     in case RM.lookup r i
		  of SOME res => RM.add(i,res @ [entry],r)
		   | NONE => RM.add(i,[entry],r)
	     end

    fun owr_rep rep (absprjid_and_funid,n,entry) : unit =
      rep := let val r = !rep
                 val i = strip_install_dir' absprjid_and_funid
		 fun owr(0,entry::res,entry') = entry'::res
		   | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		   | owr _ = die "owr_rep.owr"
	     in case RM.lookup r i
		  of SOME res => RM.add(i,owr(n,res,entry),r)
		   | NONE => die "owr_rep.NONE"
	     end

    val lookup_elab = lookup_rep elabRep #5 
    val add_elab = add_rep elabRep
    val owr_elab = owr_rep elabRep

    fun recover() =
      List.app 
      (List.app (fn entry => List.app Name.mark_gen (#5 entry)))
      (RM.range (!elabRep))

  end
