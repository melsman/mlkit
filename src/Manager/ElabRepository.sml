
functor ElabRepository(structure Name : NAME
		       structure FinMap : FINMAP
		       structure TyName : TYNAME
		       structure InfixBasis : INFIX_BASIS
		       structure OpacityEnv : OPACITY_ENV
		       structure Flags : FLAGS
		       eqtype funid
		       type ElabBasis
		       type longstrid
                       eqtype prjid
		       structure Crash : CRASH) : ELAB_REPOSITORY =
  struct

    type name = Name.name
     and InfixBasis = InfixBasis.Basis
     and funid = funid
     and ElabBasis = ElabBasis
     and opaq_env = OpacityEnv.opaq_env
     and longstrid = longstrid

    val region_profiling : bool ref = Flags.lookup_flag_entry "region_profiling"

    structure TyName = TyName

    fun die s = Crash.impossible ("ElabRepository."^s)

    val empty_infix_basis : InfixBasis = InfixBasis.emptyB
    val empty_opaq_env : opaq_env = OpacityEnv.empty

    type prjid = prjid (* was string; mads *)
    type elabRep = ((prjid * funid) * bool, (InfixBasis * ElabBasis * longstrid list * (opaq_env * TyName.Set.Set) * name list * 
					     InfixBasis * ElabBasis * opaq_env) list) FinMap.map ref
      (* the bool is true if profiling is enabled *)

    val elabRep : elabRep = ref FinMap.empty

    fun clear() = elabRep := FinMap.empty

    fun delete_rep rep prjid_and_funid = case FinMap.remove ((prjid_and_funid, !region_profiling), !rep)
					   of SOME res => rep := res
					    | _ => ()
    fun delete_entries prjid_and_funid = delete_rep elabRep prjid_and_funid

    fun lookup_rep rep exportnames_from_entry prjid_and_funid =
      let val all_gen = foldr (fn (n, b) => b andalso
			       Name.is_gen n) true
	  fun find ([], n) = NONE
	    | find (entry::entries, n) = 
	    if (all_gen o exportnames_from_entry) entry then SOME(n,entry)
	    else find(entries,n+1)
      in case FinMap.lookup (!rep) (prjid_and_funid, !region_profiling)
	   of SOME entries => find(entries, 0)
	    | NONE => NONE
      end

    fun add_rep rep (prjid_and_funid,entry) : unit =
      rep := let val r = !rep 
                 val i = (prjid_and_funid, !region_profiling)
	     in case FinMap.lookup r i
		  of SOME res => FinMap.add(i,res @ [entry],r)
		   | NONE => FinMap.add(i,[entry],r)
	     end

    fun owr_rep rep (prjid_and_funid,n,entry) : unit =
      rep := let val r = !rep
                 val i = (prjid_and_funid, !region_profiling)
		 fun owr(0,entry::res,entry') = entry'::res
		   | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		   | owr _ = die "owr_rep.owr"
	     in case FinMap.lookup r i
		  of SOME res => FinMap.add(i,owr(n,res,entry),r)
		   | NONE => die "owr_rep.NONE"
	     end

    val lookup_elab = lookup_rep elabRep #5 
    val add_elab = add_rep elabRep
    val owr_elab = owr_rep elabRep

    fun recover() =
      List.app 
      (List.app (fn entry => List.app Name.mark_gen (#5 entry)))
      (FinMap.range (!elabRep))

  end
