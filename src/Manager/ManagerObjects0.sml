(* COMPILER_ENV is the lambda env mapping structure and value
 * identifiers to lambda env's and lvars *)

(* COMPILE_BASIS is the combined basis of all environments in
 * the backend *)

functor ManagerObjects0(structure Execution : EXECUTION)
        : MANAGER_OBJECTS0 =
  struct
    structure PP = PrettyPrint
    structure TopdecGrammar = PostElabTopdecGrammar
    structure CompileBasis = Execution.CompileBasis
    structure Labels = AddressLabels

    fun die s = Crash.impossible("ManagerObjects." ^ s)
    fun chat s = if !Flags.chat then print (s ^ "\n") else ()

    type StringTree = PP.StringTree

    type absprjid = ModuleEnvironments.absprjid

    type funid = FunId.funid

    type ElabEnv = ModuleEnvironments.Env
    type CEnv = CompilerEnv.CEnv
    type CompileBasis = CompileBasis.CompileBasis

    type strid = ModuleEnvironments.strid
    type sigid = ModuleEnvironments.sigid

    type ElabBasis = ModuleEnvironments.Basis
    type InfixBasis = InfixBasis.Basis
    type opaq_env = OpacityElim.opaq_env

    type md5 = string
    type BodyBuilderClos = {infB: InfixBasis,
			    elabB: ElabBasis,
			    absprjid: absprjid,
                            filetext: string,
			    filemd5: md5,
			    opaq_env: opaq_env,
			    T: TyName.TyName list,
			    resE: ElabEnv}

    datatype IntSigEnv = ISE of TyName.Set.Set SigId.Map.map
    datatype IntFunEnv = IFE of (absprjid * strid * ElabEnv * BodyBuilderClos * IntBasis) FunId.Map.map
         and IntBasis = IB of IntFunEnv * IntSigEnv * CEnv * CompileBasis

    (* Instead of storing structure expressions in functor environments, information necessary for recreating
     * structure expressions is stored (BodyBuilderClos). *)

    local
	fun layoutBBC bbc =
	    PP.NODE{start="BBC=<todo",finish=">", indent=2,
		    childsep=PP.RIGHT", ", children=[]}
    in
	fun layoutIntSigEnv (ISE ise) = SigId.Map.layoutMap{start="IntSigEnv = [", eq="->",sep=", ", finish="]"}
	    (PP.LEAF o SigId.pr_SigId)
	  (TyName.Set.layoutSet {start="{",finish="}",sep=", "} (PP.LEAF o TyName.pr_TyName)) ise

	fun layoutIntBasis (IB(ife,ise,ce,cb)) =
	    PP.NODE{start="IntBasis = [", finish="]", indent=1, childsep=PP.RIGHT ", ",
		    children=[layoutIntFunEnv ife,
			      layoutIntSigEnv ise,
			      CompilerEnv.layoutCEnv ce,
			      CompileBasis.layout_CompileBasis cb]}
	and layoutClos a =
	    let val (absprjid, strid, E, BBC, ib) = a
	    in PP.NODE{start="<", finish=">",
		       childsep=PP.RIGHT", ", indent=2,
		       children=[layoutBBC BBC,
				 layoutIntBasis ib]}
	    end
	and layoutIntFunEnv (IFE ife) = FunId.Map.layoutMap{start="IntFunEnv = [", eq="->",sep=", ", finish="]"}
	    (PP.LEAF o FunId.pr_FunId) layoutClos ife

    end

    (* Picklers *)

    fun puSay s (p : 'a Pickle.pu) : 'a Pickle.pu =
        Pickle.comment s p


    val pu_BodyBuilderClos =
	let fun to ((infB,elabB,absprjid),(opaq_env,T),(resE,filemd5,filetext)) =
		{infB=infB,elabB=elabB,absprjid=absprjid,filemd5=filemd5,
		 opaq_env=opaq_env,T=T,resE=resE,filetext=filetext}
	    fun from {infB=infB,elabB=elabB,absprjid=absprjid,filemd5=filemd5,filetext,
		      opaq_env=opaq_env,T=T,resE=resE} = ((infB,elabB,absprjid),(opaq_env,T),(resE,filemd5,filetext))
	in Pickle.convert (to,from)
	                  (Pickle.tup3Gen0(Pickle.tup3Gen0(puSay "IBasis" InfixBasis.pu,
                                                           puSay "MEnv" ModuleEnvironments.B.pu,
                                                           puSay "absprjid" ModuleEnvironments.pu_absprjid),
			                   Pickle.pairGen0(puSay "OEnv" OpacityEnv.pu,
                                                           puSay "T" (Pickle.listGen TyName.pu)),
			                   Pickle.tup3Gen0(puSay "Env" Environments.E.pu,
                                                           puSay "hash" Pickle.string,
                                                           puSay "filetext" Pickle.string)))
	end

    val pu_IntSigEnv =
        Pickle.convert (ISE, fn ISE v => v)
	(SigId.Map.pu SigId.pu (TyName.Set.pu TyName.pu))

    val (pu_IntFunEnv,pu_IntBasis) =
	let fun IntFunEnvToInt _ = 0
	    fun IntBasisToInt _ = 0
	    fun fun_IFE (pu_IntFunEnv, pu_IntBasis) =
		Pickle.con1 IFE (fn IFE a => a)
		(FunId.Map.pu FunId.pu
			    (Pickle.convert (fn ((a,b),(d,e,f)) => (a,b,d,e,f), fn (a,b,d,e,f) => ((a,b),(d,e,f)))
			                    (Pickle.pairGen0(Pickle.pairGen0(ModuleEnvironments.pu_absprjid,StrId.pu),
					                     Pickle.tup3Gen0(Environments.E.pu,
							                     pu_BodyBuilderClos,pu_IntBasis)))))
	    fun fun_IB (pu_IntFunEnv, pu_IntBasis) =
		Pickle.con1 IB (fn IB a => a)
		(Pickle.tup4Gen0(pu_IntFunEnv,pu_IntSigEnv,CompilerEnv.pu,CompileBasis.pu))
	in Pickle.data2Gen("IntFunEnv",IntFunEnvToInt,[fun_IFE],
			   "IntBasis",IntBasisToInt,[fun_IB])
	end

    structure IntFunEnv =
      struct
	val empty = IFE FunId.Map.empty
	val initial = IFE FunId.Map.empty
	fun plus (IFE ife1, IFE ife2) = IFE(FunId.Map.plus(ife1,ife2))
	fun add (funid,e,IFE ife) = IFE(FunId.Map.add(funid,e,ife))
	fun lookup (IFE ife) funid =
	  case FunId.Map.lookup ife funid
	    of SOME res => res
	     | NONE => die ("IntFunEnv.lookup: could not find funid " ^ FunId.pr_FunId funid)
	fun restrict (IFE ife, funids) = IFE
	  (foldl (fn (funid, acc) =>
		  case FunId.Map.lookup ife funid
		    of SOME e => FunId.Map.add(funid,e,acc)
		     | NONE => die ("IntFunEnv.restrict: could not find funid " ^ FunId.pr_FunId funid))
	   FunId.Map.empty funids)
	fun enrich (IFE ife0, IFE ife) : bool = (* using md5 checksums; enrichment for free variables is checked *)
	    FunId.Map.Fold(fn ((funid, obj), b) => b andalso         (* when the functor is being declared!! *)
		      case FunId.Map.lookup ife0 funid
			of SOME obj0 =>
			    #1 obj = #1 obj0                               (* absprjid *)
			    andalso #filemd5 (#4 obj) = #filemd5 (#4 obj0) (* md5 of functor body *)
			 | NONE => false) true ife

	val layout = layoutIntFunEnv
	fun fold f i (IFE ife) = FunId.Map.Fold f i ife
	val pu = puSay "IntFunEnv" pu_IntFunEnv
      end


    structure IntSigEnv =
      struct
	val empty = ISE SigId.Map.empty
	val initial = empty
	fun plus (ISE ise1, ISE ise2) = ISE(SigId.Map.plus(ise1,ise2))
	fun add (sigid,T,ISE ise) = ISE(SigId.Map.add(sigid,T,ise))
	fun lookup (ISE ise) sigid =
	  case SigId.Map.lookup ise sigid
	    of SOME T => T
	     | NONE => die ("IntSigEnv.lookup: could not find sigid " ^ SigId.pr_SigId sigid)
	fun restrict (ISE ise, sigids) = ISE
	  (foldl (fn (sigid, acc) =>
		  case SigId.Map.lookup ise sigid
		    of SOME e => SigId.Map.add(sigid,e,acc)
		     | NONE => die ("IntSigEnv.restrict: could not find sigid " ^ SigId.pr_SigId sigid))
	   SigId.Map.empty sigids)
	fun enrich (ISE ise0, ISE ise) : bool =
	  SigId.Map.Fold(fn ((sigid, T), b) => b andalso
		      case SigId.Map.lookup ise0 sigid
			of SOME T0 => TyName.Set.eq T T0
			 | NONE => false) true ise
	val layout = layoutIntSigEnv
	fun tynames (ISE ise) = SigId.Map.fold (fn (a,b) => TyName.Set.union a b) TyName.Set.empty ise

	val pu = puSay "IntSigEnv" pu_IntSigEnv
      end


    type longid = Ident.longid
    type longstrid = StrId.longstrid
    type longtycon = TyCon.longtycon
    structure IntBasis =
      struct
	val mk = IB
	fun un (IB ib) = ib
	val empty = IB (IntFunEnv.empty, IntSigEnv.empty, CompilerEnv.emptyCEnv, CompileBasis.empty)
	fun plus (IB(ife1,ise1,ce1,cb1), IB(ife2,ise2,ce2,cb2)) =
	  IB(IntFunEnv.plus(ife1,ife2), IntSigEnv.plus(ise1,ise2), CompilerEnv.plus(ce1,ce2), CompileBasis.plus(cb1,cb2))

        fun pp_tynames s tns =
           (print ("\n" ^ s ^ ":\n  ");
	    print (String.concatWith "," (map TyName.pr_TyName tns) ^ "\n"))

	fun restrict0 (IB(ife,ise,ce,cb), {funids, sigids, longstrids, longvids, longtycons}, tynames) =
	    let val ife' = IntFunEnv.restrict(ife,funids)
	        val ise' = IntSigEnv.restrict(ise,sigids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
(*
		val _ = if !Flags.chat then (print("\n RESTRICTED(0) CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()
		val _ = pp_tynames "Restricting ce to tynames" tynames
*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		fun tynames_ife(IFE ife) =
		  let fun tynames_obj ((_,_,_,_,obj),tns) =
		        let val IB(_,ise,ce,_) = obj
			in TyName.Set.list(IntSigEnv.tynames ise) @ (CompilerEnv.tynamesOfCEnv ce @ tns)
			end
		  in FunId.Map.fold tynames_obj nil ife
		  end
		val tynames =
		    TyName.Set.union
		    (IntSigEnv.tynames ise')
		    (TyName.Set.fromList
		     (CompilerEnv.tynamesOfCEnv ce'
		      @ tynames_ife ife'
		      @ tynames))
		val tynames = TyName.Set.list
		    (TyName.Set.difference tynames
		     (TyName.Set.fromList TyName.tynamesPredefined))
		val cons = CompilerEnv.consOfCEnv ce'
		val excons = CompilerEnv.exconsOfCEnv ce'
		val cb' = CompileBasis.restrict0(cb,(lvars,tynames,cons,excons))

	    (* because of the delayed interpretation of functors, we
	     * need also add the compiler bases for each of the functor
	     * identifiers to the resulting interpretation basis; see
	     * the example test/fxp_err.sml for an example where not
	     * adding compiler bases causes the compiler to crash. *)

(*I don't udnerstand this; mael 2004-11-24
		val cb'' = IntFunEnv.fold (fn ((_,functorClos),cb) =>
					   let val IB ib = #6 functorClos
					   in CompileBasis.plus(cb,#4 ib)
					   end) cb' ife'
*)
		val cb'' = cb'
	    in IB (ife',ise',ce',cb'')
	    end

	fun restrict (ib, ids, tynames) =
	    let val tynames = [TyName.tyName_EXN,     (* exn is used explicitly in CompileDec *)
			       TyName.tyName_INT31,   (* int31, int32, int63, int64, intinf, word8, word31, *)
			       TyName.tyName_INT32,   (*  word32, word63, word64 needed because of overloading *)
			       TyName.tyName_INT63,
			       TyName.tyName_INT64,
			       TyName.tyName_INTINF,
			       TyName.tyName_WORD8,
			       TyName.tyName_WORD31,
			       TyName.tyName_WORD32,
			       TyName.tyName_WORD63,
			       TyName.tyName_WORD64,
			       TyName.tyName_STRING,  (* string is needed for string constants *)
			       TyName.tyName_CHAR,    (* char is needed for char constants *)
			       TyName.tyName_REF,
			       TyName.tyName_REAL,    (* real needed because of overloading *)
			       TyName.tyName_F64]     (* f64 needed because of optimiser *)
		  @ TyName.Set.list tynames
		val IB(ife,ise,ce,cb) = ib
		val {funids, sigids, longstrids, longvids, longtycons} = ids
		val longstrids = StrId.mk_LongStrId ["IntInfRep"] :: longstrids
		val ife' = IntFunEnv.restrict(ife,funids)
	        val ise' = IntSigEnv.restrict(ise,sigids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
(*
		val _ = if !Flags.chat then (print("\n RESTRICTED CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()
*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		fun tynames_ife(IFE ife) =
		  let fun tynames_obj ((_,_,_,_,obj),tns) =
		        let val IB(_,ise,ce,_) = obj
			in TyName.Set.list(IntSigEnv.tynames ise) @ (CompilerEnv.tynamesOfCEnv ce @ tns)
			end
		  in FunId.Map.fold tynames_obj nil ife
		  end
		val tynames =
		    TyName.Set.union
		    (IntSigEnv.tynames ise')
		    (TyName.Set.fromList
		     (CompilerEnv.tynamesOfCEnv ce'
		      @ tynames_ife ife'
		      @ tynames))
		val tynames = TyName.Set.list tynames
		val cons = CompilerEnv.consOfCEnv ce'
		val excons = CompilerEnv.exconsOfCEnv ce'
		val cb' = CompileBasis.restrict(cb,(lvars,tynames,cons,excons))

	    (* because of the delayed interpretation of functors, we
	     * need also add the compiler bases for each of the functor
	     * identifiers to the resulting interpretation basis; see
	     * the example test/fxp_err.sml for an example where not
	     * adding compiler bases causes the compiler to crash. *)
		val cb'' = IntFunEnv.fold (fn ((_,functorClos),cb) =>
					   let val IB ib = #5 functorClos
					   in CompileBasis.plus(cb,#4 ib)
					   end) cb' ife'

	    in IB (ife',ise',ce',cb'')
	    end

	fun match (IB(ife1,ise1,ce1,cb1),IB(ife2,ise2,ce2,cb2)) =
	  let val _ = CompilerEnv.match(ce1,ce2)
	      val cb1' = CompileBasis.match(cb1,cb2)
	  in IB(ife1,ise1,ce1,cb1')
	  end

	local
	    fun db_f s true = true
	      | db_f s false = false before print ("IntBasis.enrich:" ^ s ^ " false\n")

	    fun IntFunEnv_enrich a = db_f "IntFunEnv" (IntFunEnv.enrich a)
	    fun IntSigEnv_enrich a = db_f "IntSigEnv" (IntSigEnv.enrich a)
	    fun CompilerEnv_enrichCEnv a = db_f "CompilerEnv" (CompilerEnv.enrichCEnv a)
	    fun CompileBasis_enrich a = db_f "CompileBasis" (CompileBasis.enrich a)
	in
	  fun enrich (IB(ife0,ise0,ce0,cb0),IB(ife,ise,ce,cb)) =
	    IntFunEnv_enrich(ife0,ife) andalso IntSigEnv_enrich(ise0,ise)
	    andalso CompilerEnv_enrichCEnv(ce0,ce) andalso CompileBasis_enrich(cb0,cb)
	end

	local
	  fun agree1 (longstrid, (_,_,ce1,cb1), (_,_,ce2,cb2)) =
	    let val ce1 = CompilerEnv.lookup_longstrid ce1 longstrid
	        val ce2 = CompilerEnv.lookup_longstrid ce2 longstrid
	    in
	      CompilerEnv.enrichCEnv(ce1,ce2) andalso CompilerEnv.enrichCEnv(ce2,ce1) andalso
	      let
		fun restr ce cb =
		  let val lvars = CompilerEnv.lvarsOfCEnv ce
		      val tynames = CompilerEnv.tynamesOfCEnv ce
		      val cons = CompilerEnv.consOfCEnv ce
		      val excons = CompilerEnv.exconsOfCEnv ce
		  in CompileBasis.restrict(cb,(lvars,tynames,cons,excons))
		  end
		val cb1 = restr ce1 cb1
		val cb2 = restr ce2 cb2
	      in CompileBasis.eq(cb1,cb2)
	      end
	    end
	  fun agree2 ([], _,_) = true
	    | agree2 (longstrid::longstrids, B1, B2) =
	    agree1(longstrid, B1, B2) andalso agree2(longstrids, B1, B2)
	in
	  fun agree (l, IB B1, IB B2) = agree2 (l, B1, B2)
	end

	val layout = layoutIntBasis

	  (* operations used in Manager, only. *)
	fun initial () = IB (IntFunEnv.initial, IntSigEnv.initial,
			    CompilerEnv.initialCEnv(), CompileBasis.initial)
	val pu = puSay "IntBasis" pu_IntBasis

	fun closure tynames dom (ib',ib) =
	    (* closure_IB'(IB) : the closure of IB w.r.t. IB' *)
	    restrict0(plus(ib',ib),dom,tynames)
      end

    datatype Basis = BASIS of InfixBasis * ElabBasis * opaq_env * IntBasis

    type longids = {funids:funid list, sigids:sigid list, longstrids: longstrid list,
		    longvids: longid list, longtycons: longtycon list}

    structure Basis =
      struct
	val empty = BASIS (InfixBasis.emptyB, ModuleEnvironments.B.empty, OpacityEnv.empty, IntBasis.empty)
	fun mk b = BASIS b
	fun un (BASIS b) = b
	fun plus (BASIS (infb,elabb,rea,intb), BASIS (infb',elabb',rea',intb')) =
	  BASIS (InfixBasis.compose(infb,infb'), ModuleEnvironments.B.plus (elabb, elabb'),
		 OpacityEnv.plus(rea,rea'), IntBasis.plus(intb, intb'))

	val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"
	fun log s = TextIO.output(TextIO.stdOut,s)
	fun debug (s, b) =
	  if !debug_man_enrich then
	    (if b then log("\n" ^ s ^ ": enrich succeeded.")
	     else log("\n" ^ s ^ ": enrich failed."); b)
	  else b
	local
	  fun InfixBasis_eq a = InfixBasis.eq a
	  fun ModuleEnvironments_B_enrich a = ModuleEnvironments.B.enrich a
	  fun OpacityElim_enrich a = OpacityEnv.enrich a
	  fun IntBasis_enrich a = IntBasis.enrich a
	in
	  fun enrich (BASIS (infB1,elabB1,rea1,tintB1), (BASIS (infB2,elabB2,rea2,tintB2), dom_rea)) =
	    debug("InfixBasis", InfixBasis_eq(infB1,infB2)) andalso
	    debug("ElabBasis", ModuleEnvironments_B_enrich (elabB1,elabB2)) andalso
	    debug("OpacityEnv", OpacityElim_enrich (rea1,(rea2,dom_rea))) andalso
	    debug("IntBasis", IntBasis_enrich(tintB1,tintB2))
	end

	fun agree (longstrids, BASIS(_,elabB1,rea1,tintB1), (BASIS(_,elabB2,rea2,tintB2), dom_rea)) =
	  ModuleEnvironments.B.agree(longstrids,elabB1,elabB2) andalso IntBasis.agree(longstrids,tintB1,tintB2)

	fun restrict (BASIS(infB,eB,oe,iB),ids: longids) =
	    let val _ = chat "[restricting elaboration basis begin...]"
		val eB' = ModuleEnvironments.B.restrict(eB,ids)
		val _ = chat "[restricting elaboration basis end...]"

		val _ = chat "[finding tynames in elaboration basis begin...]"
		val tynames_eB' = ModuleEnvironments.B.tynames eB'
		val _ = chat "[finding tynames in elaboration basis end...]"

		val _ = chat "[restricting opacity env begin...]"
		val oe' = OpacityEnv.restrict(oe,(#funids ids,tynames_eB'))
		val tynames_oe' = StatObject.Realisation.tynamesRng(OpacityEnv.rea_of oe')
		val tynames = TyName.Set.union tynames_oe' tynames_eB'
		val _ = chat "[restricting opacity env end...]"

		val _ = chat "[restricting interpretation basis begin...]"
		val iB' = IntBasis.restrict(iB,ids,tynames_oe')
		val _ = chat "[restricting interpretation basis end...]"
	    in (BASIS(infB,eB',oe',iB'),tynames)
	    end

	fun match (BASIS(infB,eB,oe,iB), BASIS(infB0,eB0,oe0,iB0)) =
	    let val _ = ModuleEnvironments.B.match(eB,eB0)
		val _ = OpacityEnv.match(oe,oe0)
		val iB = IntBasis.match(iB,iB0)
	    in BASIS(infB,eB,oe,iB)
	    end

	fun domain (BASIS(_,eB,_,_)) : longids = ModuleEnvironments.B.domain eB

	fun db_f s true = true
	  | db_f s false = false before print ("Basis.eq:" ^ s ^ " false\n")

	fun eq (BASIS(infB1,eB1,oe1,iB1), BASIS(infB2,eB2,oe2,iB2)) =
	    db_f "InfixBasis" (InfixBasis.eq(infB1,infB2)) andalso
	    db_f "B_l" (ModuleEnvironments.B.enrich(eB1,eB2)) andalso db_f "B_r" (ModuleEnvironments.B.enrich(eB2,eB1)) andalso
	    db_f "OpacityEnv" (OpacityEnv.eq(oe1,oe2)) andalso
	    db_f "IB_l" (IntBasis.enrich(iB1,iB2)) andalso db_f "IB_r" (IntBasis.enrich(iB2,iB1))

	fun layout (BASIS(infB,elabB,rea,intB)) : StringTree =
	  PP.NODE{start="BASIS(", finish = ")",indent=1,childsep=PP.RIGHT ", ",
		  children=[InfixBasis.layoutBasis infB, ModuleEnvironments.B.layout elabB,
			    OpacityEnv.layout rea, IntBasis.layout intB]}

	fun closure (B': Basis, B: Basis) : Basis =
	    (* closure_B'(B) : the closure of B w.r.t. B' *)
	    let val BASIS(infB',eB',oe',iB') = B'
		val BASIS(infB,eB,oe,iB) = B
(*
                val _ = if !Flags.chat then
                      (print "\nClosure_B'(B):\n";
                       print "B' = \n";
                       PP.printTree (layout B');
                       print "\nB = \n";
                       PP.printTree (layout B);
                       print "\n")
                     else ()
*)
		val dom = domain B
		fun subtractPredefinedTynames tns =
		    TyName.Set.difference tns (TyName.Set.fromList TyName.tynamesPredefined)
		val tynames_eB = subtractPredefinedTynames(ModuleEnvironments.B.tynames eB)
		val oe1 = OpacityEnv.plus(oe',oe)
		val oe2 = OpacityEnv.restrict(oe1,(#funids dom,tynames_eB))

		val tynames_oe2 = StatObject.Realisation.tynamesRng(OpacityEnv.rea_of oe2)
		val tynames = TyName.Set.union tynames_oe2 tynames_eB

		val iBclosed = IntBasis.closure (TyName.Set.list tynames_oe2) dom (iB',iB)
	    in
		BASIS(infB,eB,oe2,iBclosed)
	    end

	fun initial () = BASIS (InfixBasis.emptyB,
			        ModuleEnvironments.B.initial(),
			        OpacityEnv.initial,
			        IntBasis.initial())
	val _ = app Name.mk_rigid (!Name.bucket)

	val pu =
	    Pickle.comment "MO.Basis"
	    (Pickle.convert (BASIS, fn BASIS a => a)
	     (Pickle.tup4Gen0(InfixBasis.pu,
			      ModuleEnvironments.B.pu,
			      Pickle.comment "OpacityEnv.pu" OpacityEnv.pu,
			      IntBasis.pu)))

	type Basis0 = InfixBasis * ElabBasis
	val pu_Basis0 =
	    puSay "Basis0" (Pickle.pairGen(InfixBasis.pu, ModuleEnvironments.B.pu))
	fun plusBasis0 ((ib,eb),(ib',eb')) =
	    (InfixBasis.compose(ib,ib'), ModuleEnvironments.B.plus(eb,eb'))
	fun initialBasis0() =
	    (InfixBasis.emptyB, ModuleEnvironments.B.initial())
	fun matchBasis0 ((infB,eB), (infB0,eB0)) =
	    let val _ = ModuleEnvironments.B.match(eB,eB0)
	    in (infB,eB)
	    end
	fun eqBasis0 ((infB1,eB1), (infB2,eB2)) =
	    db_f "InfixBasis" (InfixBasis.eq(infB1,infB2)) andalso
	    db_f "B_l" (ModuleEnvironments.B.enrich(eB1,eB2)) andalso db_f "B_r" (ModuleEnvironments.B.enrich(eB2,eB1))

	type Basis1 = opaq_env * IntBasis
	val pu_Basis1 =
	    puSay "Basis1" (Pickle.pairGen(OpacityEnv.pu, IntBasis.pu))
	fun plusBasis1 ((oe,ib),(oe',ib')) =
	    (OpacityEnv.plus(oe,oe'),
	     IntBasis.plus(ib,ib'))
	fun initialBasis1 () = (OpacityEnv.initial,
			        IntBasis.initial())
	fun matchBasis1 ((oe,iB), (oe0,iB0)) =
	    let val _ = OpacityEnv.match(oe,oe0)
		val iB = IntBasis.match(iB,iB0)
	    in (oe,iB)
	    end
	fun eqBasis1 ((oe1,iB1), (oe2,iB2)) =
	    db_f "OpacityEnv" (OpacityEnv.eq(oe1,oe2)) andalso
	    db_f "IB_l" (IntBasis.enrich(iB1,iB2)) andalso db_f "IB_r" (IntBasis.enrich(iB2,iB1))

      end

    type name = Name.name


    datatype cval = datatype Execution.cval
    fun retrieve_longid (B: Basis) (longid: Ident.longid) : string cval =
        let val (_,_,_,iB) = Basis.un B
            val (_,_,CE,CB) = IntBasis.un iB
        in Execution.retrieve_longid CE CB longid
        end

  end
