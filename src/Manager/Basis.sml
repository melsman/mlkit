(*$Basis: INFIX_BASIS MODULE_ENVIRONMENTS COMPILE_BASIS BASIS FREE_IDS
    NAME PRETTYPRINT TYNAME FLAGS*)
 
functor Basis(structure InfixBasis: INFIX_BASIS
	      structure ModuleEnvironments: MODULE_ENVIRONMENTS
	      structure TyName: TYNAME
		sharing TyName = ModuleEnvironments.TyName
	      structure CompileBasis: COMPILE_BASIS
		sharing type ModuleEnvironments.TyName = CompileBasis.TyName
	      structure FreeIds : FREE_IDS
		sharing type FreeIds.ids = CompileBasis.ids
		    and type FreeIds.id = ModuleEnvironments.id
		    and type FreeIds.tycon = ModuleEnvironments.tycon
		    and type FreeIds.strid = ModuleEnvironments.strid
		    and type FreeIds.funid = ModuleEnvironments.funid
		    and type FreeIds.sigid = ModuleEnvironments.sigid
	      structure Name : NAME
	      structure PP : PRETTYPRINT
		sharing type PP.StringTree = InfixBasis.StringTree
		  = ModuleEnvironments.StringTree = CompileBasis.StringTree
              structure Flags : FLAGS
	     ): BASIS =
  struct


    val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"

    fun log s = output(std_out,s)

    type InfixBasis   = InfixBasis.Basis
    type StaticBasis  = ModuleEnvironments.Basis
    type CompileBasis = CompileBasis.CompileBasis

    type Basis = InfixBasis * StaticBasis * CompileBasis

    val emptyB = (InfixBasis.emptyB,
		  ModuleEnvironments.B.empty,
		  CompileBasis.empty)

    val initialB = (InfixBasis.emptyB,
		    ModuleEnvironments.B.initial,
		    CompileBasis.initial)

    fun Inf_in_B iBas = (iBas, ModuleEnvironments.B.empty, CompileBasis.empty)
    fun Stat_in_B sBas = (InfixBasis.emptyB, sBas, CompileBasis.empty)
    fun Comp_in_B cBas = (InfixBasis.emptyB, ModuleEnvironments.B.empty, cBas)

    fun Inf_of_B(iBas, _, _) = iBas
    fun Stat_of_B(_, sBas, _) = sBas
    fun Comp_of_B(_, _, cBas) = cBas

    fun B_plus_B((iBas1, sBas1, cBas1), (iBas2, sBas2, cBas2)) = 
      (InfixBasis.compose(iBas1, iBas2),
       ModuleEnvironments.B.plus (sBas1, sBas2),
       CompileBasis.plus(cBas1, cBas2))

    type ids = FreeIds.ids

    fun restrict_sBas(sBas,ids:ids) =
      ModuleEnvironments.B.restrict (sBas,{ids=FreeIds.vids_of_ids ids,
					   tycons=FreeIds.tycons_of_ids ids,
					   strids=FreeIds.strids_of_ids ids,
					   funids=FreeIds.funids_of_ids ids,
					   sigids=FreeIds.sigids_of_ids ids})

    fun restrict ((iBas,sBas,cBas), ids) = 
      let val sBas' = restrict_sBas(sBas,ids)
	  val tynames =
	        TyName.Set.list
	          (TyName.Set.union
		     (TyName.Set.fromList
			[TyName.tyName_EXN,     (* exn is used explicitly in CompileDec *)
			 TyName.tyName_INT,     (* int needed because of overloading *)
                         TyName.tyName_STRING,  (* string is needed for string constants *)
			 TyName.tyName_REAL])   (* real needed because of overloading *)
		     (ModuleEnvironments.B.tynames sBas'))
 val cBas' = CompileBasis.restrict(cBas,ids,tynames)
      in (iBas, sBas',cBas') (*don't restrict iBas*)
      end
			
    fun debug(s, b) = if !debug_man_enrich then
                         (if b then log("\n" ^ s ^ ": enrich succeeded.")
			  else log("\n" ^ s ^ ": enrich failed."); b)
		      else b

    fun enrich ((iBas1,sBas1,cBas1),(iBas2,sBas2,cBas2)) = 
      debug("infixbasis",   InfixBasis.eq(iBas1,iBas2)) andalso 
      debug("statbasis",    ModuleEnvironments.B.enrich (sBas1,sBas2)) andalso
      debug("compilebasis", CompileBasis.enrich(cBas1,cBas2))

    fun eq(B,B') = enrich(B,B') andalso enrich(B',B)

    type name = Name.name
    type ExpBasis = name list * Basis

    fun mk_ExpBasis a = a
    fun de_ExpBasis a = a

    fun match ((N,B), (N0,B0)) : ExpBasis = (*make (N1,B1) look like (N,B)*)
          let val (iBas, sBas, cBas) = B
	      val (iBas0, sBas0, cBas0) = B0

	      val _ = List.apply Name.mark_gen N
	      val _ = List.apply Name.mark_gen N0

	      val _ = ModuleEnvironments.B.match (sBas, sBas0)
	      val cBas = CompileBasis.match (cBas, cBas0)
	       
	      val _ = List.apply Name.unmark_gen N0
	      val _ = List.apply Name.unmark_gen N
	  in
	    (N, (iBas, sBas, cBas))
	  end

    fun B_plus_Bexp (B, (N',B')) = B_plus_B(B,B')
    fun Bexp_plus_Bexp ((N, B), (N',B')) = (N @ N', B_plus_B(B,B'))

    open PP

    val layout_InfixBasis = InfixBasis.layoutBasis
    val layout_StaticBasis = ModuleEnvironments.B.layout
    val layout_CompileBasis = CompileBasis.layout_CompileBasis

    fun layout_Basis (iBas,sBas,cBas) =
      NODE{start="BASIS(", finish = ")",indent=1,childsep=RIGHT ", ",
	   children=[layout_InfixBasis iBas, layout_StaticBasis sBas,
		     layout_CompileBasis cBas]}
	   
  end;
