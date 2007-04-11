
functor IntModules(structure ManagerObjects : MANAGER_OBJECTS
		       where type absprjid = string
		   structure Execution : EXECUTION
		   sharing type Execution.CompileBasis = ManagerObjects.CompileBasis
		   sharing type Execution.target = ManagerObjects.target
		   sharing type Execution.linkinfo = ManagerObjects.linkinfo)
    : INT_MODULES =
  struct
    structure CompileBasis = Execution.CompileBasis
    structure PP = PrettyPrint
    structure TopdecGrammar = PostElabTopdecGrammar
    structure ElabInfo = AllInfo.ElabInfo
    structure FunStamp = ManagerObjects.FunStamp
    structure IntBasis = ManagerObjects.IntBasis
    structure IntFunEnv = ManagerObjects.IntFunEnv
    structure IntSigEnv = ManagerObjects.IntSigEnv
    structure ModCode = ManagerObjects.ModCode
    structure CE = CompilerEnv
    structure ElabEnv = Environments.E
    structure ElabBasis = ModuleEnvironments.B
    type BodyBuilderClos = ManagerObjects.BodyBuilderClos
    type TyName = TyName.TyName
    type realisation = Environments.realisation
    type InfixBasis = ManagerObjects.InfixBasis
    type ElabEnv = ManagerObjects.ElabEnv
    type ElabBasis = ManagerObjects.ElabBasis

    fun die s = Crash.impossible ("IntModules." ^ s)
    fun print_error_report report = Report.print' report (!Flags.log)
    fun log (s:string) : unit = TextIO.output (!Flags.log, s)
    fun chat s = if !Flags.chat then log (s ^ "\n") else ()

    fun withFile (f:TextIO.instream->'a) (s:string) : 'a =
	let val is = TextIO.openIn s
	in let val a = f is
	   in TextIO.closeIn is ; a
	   end handle ? => (TextIO.closeIn is ; raise ?)
	end 

    fun writeFile f t =
        let val os = TextIO.openOut f
        in ( TextIO.output(os, t)
           ; TextIO.closeOut os) handle ? => (TextIO.closeOut os; raise ?)
        end

    fun pr_tynames [] = ""
      | pr_tynames [t] = TyName.pr_TyName t
      | pr_tynames (t::ts) = TyName.pr_TyName t ^ ", " ^ pr_tynames ts

    fun print_basis B =
      PP.outputTree(print,ModuleEnvironments.B.layout B, 100)

    fun print_intbasis ib =
      PP.outputTree(print,IntBasis.layout ib, 100)

    (* For statistics *)
    fun out_functor_application s =
      let val functor_application_file = "/tmp/functor_apps.txt"
	  val os = TextIO.openAppend functor_application_file
      in TextIO.output(os, s ^ "\n");
	TextIO.closeOut os
      end

    type IntBasis = ManagerObjects.IntBasis
     and topdec = TopdecGrammar.topdec
     and modcode = ManagerObjects.modcode
     and CompileBasis = CompileBasis.CompileBasis
     and CEnv = CE.CEnv
     and IntFunEnv = ManagerObjects.IntFunEnv

    type absprjid = ModuleEnvironments.absprjid

    open TopdecGrammar (*declares StrId*)

    fun to_TypeInfo i =
      case ElabInfo.to_TypeInfo i
	of SOME ti => SOME(ElabInfo.TypeInfo.normalise ti)
	 | NONE => NONE 

    (* For path operations *)
    infix ##
    val op ## = OS.Path.concat


    (* ----------------------------------------------------
     * Fresh unit names; in case functors `splits' sources
     * ---------------------------------------------------- *)

    local val counter = ref 0
          fun inc() = (counter := !counter + 1; !counter)
    in fun fresh_unitname() = "code" ^ Int.toString(inc())
       fun reset_unitname_counter() = counter := 0
    end

    (* ----------------------------------------------------
     * Determine where to put target files
     * ---------------------------------------------------- *)
    val mlbdir = ManagerObjects.mlbdir

    type funid = TopdecGrammar.funid
    type strexp = TopdecGrammar.strexp

    fun on_ElabInfo phi i = 
       case to_TypeInfo i of 
	   SOME i' => ElabInfo.plus_TypeInfo i (ElabInfo.TypeInfo.on_TypeInfo(phi,i'))
	 | NONE => i


    (* --------------------------------------------------------------
     * Now, how do we interpret a functor binding? It is too
     * expensive to keep the entire functor body in memory
     * throughout the lifetime of the functor. 
     *
     *   Instead, we memorize just enough information to make it
     * possible to rebuild the functor body (with all type info,
     * etc.) at the point of an application of the functor. 
     *
     *   At declaration time we write the ML source for the functor
     * body to disk (in the file funid.bdy.) Then, at application
     * time we rebuild the elaborated structure expression. The
     * information we need memorize to do this include
     *
     *    (1) the infix-basis in which the body was previously 
     *        resolved;
     *    (2) the elaboration-basis in which the body was
     *        previously elaborated;
     *    (3) the result of previously elaborating the body
     *        together with the set of type names generated
     *        while elaborating the body (to perform 
     *        matching);
     *    (4) the realisation used for performing opacity-
     *        elimination.
     * ------------------------------------------------------------ 
     *)
	       
    (* Extract a structure expression from a top-level (structure) declaration. *)
    fun extract_strexp_from_topdec(funid,topdec) =
      case topdec
	of STRtopdec(i,strdec,NONE) =>
	  (case strdec
	     of STRUCTUREstrdec(_,strbind) =>
	       (case strbind
		  of STRBIND(_,_,strexp,NONE) => strexp  (* we could check to see if funid is strid here. *)
		   | _ => die "extract_strexp_from_topdec.no (or more than one) structure binding")
	      | _ => die "extract_strexp_from_topdec.no structure binding")
	 | _ => die "extract_strexp_from_topdec.no (or more than one) structure declaration"

    type derived_form = ElabEnv option
   
    fun derived_form_repair(df,strid_arg,topdec) =
      case df of 
	  NONE => topdec
	| SOME E => 
	  (case topdec
	     of STRtopdec(i1,STRUCTUREstrdec(i2,STRBIND(i3,strid,strexp,NONE)),NONE) =>
	       let val open_info = ElabInfo.plus_TypeInfo i1 (*say*) 
	                (ElabInfo.TypeInfo.OPEN_INFO
			 let val (SE,TE,VE) = ElabEnv.un E
			 in (EqSet.list (Environments.SE.dom SE), EqSet.list (Environments.TE.dom TE), 
			     EqSet.list (Environments.VE.dom VE))
			 end)
		   val open_dec = DecGrammar.OPENdec(open_info, 
						     [WITH_INFO (open_info, 
								 StrId.longStrIdOfStrId strid_arg)])
		   val strexp' = LETstrexp(i2,DECstrdec(i1,open_dec),strexp)
	       in STRtopdec(i1,STRUCTUREstrdec(i2,STRBIND(i3,strid,strexp',NONE)),NONE)
	       end
	      | _ => die "derived_form_repair.topdec is not a single structure binding.")


    fun reelaborate (funid: funid, strid: StrId.strid, BBC : BodyBuilderClos) =
	     (let (* If the structure identifier is invented then we are
		   * dealing with the derived form of a functor
		   * binding. We handle this by modifying the elaboration
		   * basis and changing the resulting top-level
		   * declaration. *)
	   
		  val (elabB, df : derived_form) =
		      if StrId.invented_StrId strid then 
			  let val E = 
			         case ElabBasis.lookup_strid (#elabB BBC) strid of 
				     SOME E => E
				   | NONE => die "reelaborate.strid not in elabB"
			  in (ElabBasis.plus_E(#elabB BBC,E), SOME E)
			  end
		      else (#elabB BBC, NONE)

	       (* To do the matching to the old result, we temporarily store the contents of
		* the Name-bucket. We then use the Name-bucket to find generated names during
		* re-elaboration. *)
	
		(* val _ = print_basis elabB *)
	         val names_old = !Name.bucket  (* remember to re-install these names *)
	         val _ = Name.bucket := []
		   
		 (* Re-elaboration *)

                 val filename = 
                     let val text = #filetext BBC
                         val basedir = OS.Path.dir (Flags.get_string_entry "output")                        
                         val filename = basedir ## (FunId.pr_FunId funid ^ ".bdy")
                     in writeFile filename text
                      ; filename
                     end
	         val res = ParseElab.parse_elab {infB= #infB BBC, elabB=elabB, 
						 absprjid= #absprjid BBC, file=filename} 
	      in case res
		   of ParseElab.FAILURE (report,error_codes) => (print_error_report report;
								 die "reelaborate.ParseElab.FAILURE")
		    | ParseElab.SUCCESS {elabB=elabB',topdec,...} =>
		     let 
			   (* We now extract the result environment out of the result basis
			    * by looking up `funid' in the structure environment. *)
			 val resE' = case ElabBasis.lookup_strid elabB' 
			                   ((StrId.mk_StrId o FunId.pr_FunId) funid)
				       of SOME E => E
					| NONE => die ("reelaborate.the builder has been applied - \
					               \but I cannot find `funid' in resulting structure env.")

			 val topdec = derived_form_repair(df, strid,topdec)
					     
			 (* Do opacity elimination again. *)
			 fun opacity_elimination a = OpacityElim.opacity_elimination a
			 val _ = chat "[opacity elimination begin...]"
			 val (topdec, opaq_env') = opacity_elimination(#opaq_env BBC, topdec)
			 val _ = chat "[opacity elimination end...]"
			 val resE' = Environments.Realisation.on_Env (OpacityEnv.rea_of opaq_env') resE'
			 val resE' = Environments.Realisation.on_Env (OpacityEnv.rea_of (#opaq_env BBC)) resE'

			 val names_elab = !Name.bucket
			 val _ = Name.bucket := names_old  (* re-install old names *)
						       
			 (* The type name set T includes type names
			  * generated during opacity elimination, see
			  * functor OpacityElim; ME 1998-11-10. *)

			 (* Now, do matching *)
			 val _ = (List.app (Name.mark_gen o TyName.name) (#T BBC);
				  List.app Name.mark_gen names_elab; 
				  Name.rematching := true;
				  ElabEnv.match(resE', #resE BBC);
				  Name.rematching := false;
				  List.app (Name.unmark_gen o TyName.name) (#T BBC);
				  List.app Name.unmark_gen names_elab)

			 (* Check that match succeeded and result environment is correct. *)
			 val _ = if ElabEnv.eq(#resE BBC,resE') then ()
				 else (print "Check on environments failed\n";
				       print "\nresE = \n"; PP.printTree (ElabEnv.layout (#resE BBC));
				       print "\nresE' = \n"; PP.printTree (ElabEnv.layout resE');
				       print ("\nT = {" ^ pr_tynames (#T BBC) ^ "}\n"); 
				       die ("reelaborate.the builder has been applied - \
					\but the result environments are not equal (after matching.)"))
					      
			 val strexp = extract_strexp_from_topdec (funid,topdec)
		     in strexp
		     end
	      end handle X => (print ("Error while reconstructing functor body for " ^ 
				      FunId.pr_FunId funid ^ "\n");
			       print (General.exnMessage X);
			       raise X)
	    ) (*let*)

   fun lookupInlineFunApp (ife:IntFunEnv) (funid:funid) 
       : strid * ElabEnv * strexp * CEnv * IntFunEnv =
       let val (absprjid,funstamp,strid,E,BBC,intB0) = IntFunEnv.lookup ife funid
	   val (ife0,_,ce0,_) = IntBasis.un intB0
       in (strid,E,reelaborate(funid,strid,BBC),ce0,ife0)
       end

    (* ----------------------------------------------------
     * Compile a sequence of structure declarations
     * ---------------------------------------------------- *)

    fun compile_strdecs(fi,intB, [], unitname_opt) = (CE.emptyCEnv, CompileBasis.empty, ModCode.empty)
      | compile_strdecs(fi,intB, strdecs, unitname_opt) =
      let val (_,_,ce,cb) = IntBasis.un intB
	  val unitname = case unitname_opt
			   of SOME unitname => unitname
			    | NONE => fresh_unitname()
	  val vcg_filename = (* pmdir() ^ *) unitname ^ ".vcg"
      in case Execution.compile ((#1 o IntBasis.un) intB, lookupInlineFunApp) (ce,cb,strdecs,vcg_filename)
	   of Execution.CodeRes(ce',cb',target,linkinfo) =>
	     (ce',cb', ModCode.mk_modcode(target,linkinfo,unitname))
	    | Execution.CEnvOnlyRes ce' => (ce', CompileBasis.empty, ModCode.empty)
      end

    (* funapp_strdec strdec  returns true if strdec 
     * contains a functor application. *) 

    fun funapp_strdec strdec =
      case strdec
	of STRUCTUREstrdec(_,strbind) => funapp_strbind strbind 
	 | SEQstrdec(_,strdec1,strdec2) => 
	  funapp_strdec strdec1 orelse funapp_strdec strdec2
	 | LOCALstrdec(_,strdec1,strdec2) => 
	  funapp_strdec strdec1 orelse funapp_strdec strdec2
	 | EMPTYstrdec _ => false
	 | DECstrdec _ => false
    and funapp_strbind (STRBIND(_,_,strexp,strbind_opt)) =
      funapp_strexp strexp orelse (case strbind_opt
				     of SOME strbind => funapp_strbind strbind
				      | NONE => false)
    and funapp_strexp strexp =
      case strexp
	of STRUCTstrexp(_,strdec) => funapp_strdec strdec
	 | TRANSPARENT_CONSTRAINTstrexp(_,strexp,_) => funapp_strexp strexp
	 | OPAQUE_CONSTRAINTstrexp(_,strexp,_) => funapp_strexp strexp
	 | APPstrexp _ => true 
	 | LONGSTRIDstrexp _ => false 
	 | LETstrexp(_,strdec,strexp) => funapp_strdec strdec orelse funapp_strexp strexp

    (* The infos are not propagated exactly in the push-functions
     * below - but we are not going to use them anyway.. - Martin *)

    fun push_topdec(SIGtopdec(i,sigdec, topdec_opt)) = 
            let val topdec_opt' = 
	          case push_topdec_opt topdec_opt
		    of (SOME strdec, topdec_opt') => SOME(STRtopdec(i,strdec,topdec_opt'))
		     | (NONE, topdec_opt') => topdec_opt'
	    in (NONE, SOME (SIGtopdec(i,sigdec, topdec_opt')))
	    end 
      | push_topdec(STRtopdec(i,strdec, topdec_opt)) =
        (case push_topdec_opt topdec_opt
	   of (SOME strdec', topdec_opt') => (SOME(SEQstrdec(i,strdec,strdec')), topdec_opt')
	    | (NONE, topdec_opt') => (SOME strdec, topdec_opt'))
      | push_topdec(FUNtopdec(i,fundec, topdec_opt)) =
	   let val topdec_opt' = 
	         case push_topdec_opt topdec_opt
		   of (SOME strdec, topdec_opt') => SOME(STRtopdec(i,strdec,topdec_opt'))
		    | (NONE, topdec_opt') => topdec_opt'
	   in (NONE, SOME (FUNtopdec(i,fundec, topdec_opt')))
	   end 
    and push_topdec_opt (SOME topdec) = push_topdec topdec
      | push_topdec_opt NONE = (NONE, NONE)

    (* strdecs_of_strdec strdec  eliminates SEQstrdec and 
     * EMPTYstrdec at upper level *)

    fun strdecs_of_strdec (strdec, a) =
      case strdec
	of EMPTYstrdec _ => a
	 | SEQstrdec(_,strdec1, strdec2) => strdecs_of_strdec(strdec1, strdecs_of_strdec(strdec2,a))
	 | _ => strdec :: a

    (* comp_int_strdec(intB, strdec): strdecs not containing functor
     * applications are sent to the compiler. *)

    fun comp_int_strdec (fi:bool,intB, strdec, unitname_opt) =
      let fun loop (intB, [], strdecs) = compile_strdecs(fi,intB, rev strdecs, unitname_opt)
	    | loop (intB, strdec::strdecs, strdecs') =
	    if not fi andalso funapp_strdec strdec then
	      let val (ce1,cb1,mc1) = compile_strdecs(fi,intB, rev strdecs', NONE)
		  val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce1,cb1)
		  val intB' = IntBasis.plus(intB,intB1)
		  val (ce2,cb2,mc2) = int_strdec(intB', strdec)
		  val intB2 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce2,cb2) 
		  val (ce3,cb3,mc3) = loop(IntBasis.plus(intB',intB2), strdecs, [])
		  val ce = CE.plus(ce1,CE.plus(ce2,ce3))
		  val cb = CompileBasis.plus(cb1,CompileBasis.plus(cb2,cb3))
		  val mc = ModCode.seq(mc1,ModCode.seq(mc2,mc3))
	      in (ce,cb,mc)
	      end
	    else loop(intB,strdecs,strdec::strdecs')
	  val strdecs = strdecs_of_strdec(strdec,[])
      in loop (intB, strdecs, [])
      end

    and int_strdec (intB: IntBasis, strdec: strdec) : CEnv * CompileBasis * modcode =
      case strdec
	of STRUCTUREstrdec(i,strbind) => int_strbind(intB,strbind)
	 | LOCALstrdec(i,strdec1,strdec2) =>
	  let val (ce1,cb1,mc1) = comp_int_strdec(false,intB,strdec1,NONE)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce1,cb1)
	      val (ce2,cb2,mc2) = comp_int_strdec(false,IntBasis.plus(intB,intB1),strdec2,NONE)
	  in (ce2, CompileBasis.plus(cb1,cb2), ModCode.seq(mc1,mc2))
	  end
	 | DECstrdec _ => die "int_strdec.DECstrdec.the compiler should deal with this"
	 | EMPTYstrdec _ => die "int_strdec.EMPTYstrdec.linearization failed"
	 | SEQstrdec _ => die "int_strdec.SEQstrdec.linearization failed"

    and int_strbind (intB, STRBIND(i, strid, strexp, strbind_opt)) =
      let val (ce,cb,mc) = int_strexp(intB, strexp)
	  val ce = CE.declare_strid(strid,ce,CE.emptyCEnv)
      in case strbind_opt
	   of SOME strbind' =>
	     let val (ce',cb',mc') = int_strbind(intB, strbind')
	     in (CE.plus(ce,ce'), CompileBasis.plus(cb,cb'), ModCode.seq(mc,mc'))
	     end
	    | NONE => (ce,cb,mc)
      end 

    and int_strexp (intB: IntBasis, strexp: strexp) : CEnv * CompileBasis * modcode =
      case strexp
	of STRUCTstrexp(i, strdec) => comp_int_strdec(false,intB, strdec,NONE)
	 | LONGSTRIDstrexp(i, longstrid) =>
	  let val (_,_,ce,_) = IntBasis.un intB
	      val (strids,strid) = StrId.explode_longstrid longstrid
	      fun lookup (ce, []) = CE.lookup_strid ce strid
		| lookup (ce, strid::strids) = lookup(CE.lookup_strid ce strid, strids)
	      val ce = lookup(ce, strids)
	  in (ce, CompileBasis.empty, ModCode.empty)
	  end
	| TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (ce, cb, mc) = int_strexp(intB, strexp)
	      val E = case to_TypeInfo i
			of SOME (ElabInfo.TypeInfo.TRANS_CONSTRAINT_INFO E) => E
			 | _ => die "int_strexp.TRANSPARENT_CONSTRAINTstrexp.no env info"
	      val ce' = CE.constrain(ce,E)
	  in (ce', cb, mc)
	  end
	| OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) => 
	   die "OPAQUE_CONSTRAINTstrexp.should be eliminated at this stage"
	| APPstrexp(i, funid, strexp) => 
	  let val _ = chat ("[interpreting functor argument to functor " ^ FunId.pr_FunId funid ^ " begin...]")

	      val (phi,Eres) = case to_TypeInfo i
				 of SOME (ElabInfo.TypeInfo.FUNCTOR_APP_INFO {rea_inst,rea_gen,Env}) => 
				   let (*val _ = print "Inst realisation is: \n"
				     val _ = PP.printTree (Environments.Realisation.layout rea_inst)
				     val _ = print "\n"
				     val _ = print "Gen realisation is: \n"
				     val _ = PP.printTree (Environments.Realisation.layout rea_gen)
				     val _ = print "\n" *)
				   in
				     (Environments.Realisation.oo(rea_inst,rea_gen), Env)
				   end
				  | _ => die "int_strexp.APPstrexp.no (phi,E) info"

	      val (ce, cb, mc) = int_strexp(intB, strexp)
	      val _ = chat "[interpreting functor argument end...]"
	      val (absprjid,funstamp,strid,E,BBC,intB0) = IntFunEnv.lookup ((#1 o IntBasis.un) intB) funid
	      val E' = Environments.Realisation.on_Env phi E
	      val _ = chat "[constraining argument begin...]" 
	      val ce = CE.constrain(ce,E')
	      val _ = chat "[constraining argument end...]"
	      val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,CE.declare_strid(strid,ce,CE.emptyCEnv),
				      CompileBasis.plus(#4 (IntBasis.un intB), cb))  (* The argument may use things *)
		                                                                     (* that are not in intB0 *)
	      fun reuse_msg s = (print("[cannot reuse instance code for functor `" 
				       ^ FunId.pr_FunId funid ^ "' -- " ^ s ^ "]\n"); NONE)

	      val _ = print("[compiling body of functor " ^ FunId.pr_FunId funid ^ 
			    " (from project " ^ ModuleEnvironments.absprjid_to_string absprjid ^ ") begin]\n")
(* 	      val _ = out_functor_application (FunId.pr_FunId funid)  (* for statistics *) *)

	      val _ = chat "[recreating functor body begin...]"
	      val strexp0 = reelaborate(funid,strid,BBC)
	      val _ = chat "[recreating functor body end...]"
	      val (intB', longstrids) = 
		  let
		      val _ = chat "[finding free identifiers begin...]"
		      val {funids,longstrids,longtycons,longvids,sigids} = FreeIds.fid_strexp strexp0
		      val _ = chat "[finding free identifiers end...]"
		      val _ = chat "[restricting interpretation basis begin...]"
		      val intB' = IntBasis.restrict(IntBasis.plus(intB0,intB1), 
						    {funids=funids,sigids=sigids,longtycons=longtycons,
						     longstrids=longstrids,longvids=longvids}, TyName.Set.empty   (* MEMO: Potential error!!! mael 2004-11-17 *)  )
		      val _ = chat "[restricting interpretation basis end...]"
		  in (intB', longstrids)
		  end
	      val strexp0' = TopdecGrammar.map_strexp_info (on_ElabInfo phi) strexp0

		     (* Before we interpret strexp0' we force generated names
		      * to be `rigid' by emptying the generative names
		      * bucket, located in Name. This might lead to some
		      * unnecessary recompilation - but for now it'll do. -
		      * Martin *)
	      val _ = List.app Name.mk_rigid (!Name.bucket)
	      val _ = Name.bucket := []
	      val _ = chat "[interpreting functor body begin...]"
	      val (ce',cb',mc') = int_strexp(intB', strexp0')
	      val _ = chat "[interpreting functor body end...]"
	      val N' = !Name.bucket
	      val _ = Name.bucket := []
	      val _ = List.app Name.mk_rigid N'
		       
	      val intB'' = IntBasis.mk(IntFunEnv.empty, IntSigEnv.empty, ce', cb')

	      val cb'' = CompileBasis.plus(cb,cb')
	      val mc'' = ModCode.seq(mc,mc')
	  in (ce', cb'', ModCode.emit (absprjid, mc''))     (* we also emit code for arg.. see comment above *)
	      before print("[compiling body of functor " ^ FunId.pr_FunId funid ^ " end]\n")
	  end

	| LETstrexp(info, strdec, strexp) => 
	  let val (ce1,cb1,mc1) = comp_int_strdec(false,intB,strdec,NONE)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce1,cb1)
	      val (ce2,cb2,mc2) = int_strexp(IntBasis.plus(intB,intB1),strexp)
	  in (ce2, CompileBasis.plus(cb1,cb2), ModCode.seq(mc1,mc2))
	  end
    
    fun generate_BBC(absprjid : absprjid, funid, strid_arg,
			      {infB: InfixBasis, elabB: ElabBasis, T: TyName list, 
			       resE: ElabEnv, opaq_env: OpacityElim.opaq_env}, strexp : strexp) 
       : BodyBuilderClos =
	   let val funid_string = FunId.pr_FunId funid
(**	       val filename = OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid))
		              ^ "." ^ funid_string ^ ".bdy" **)
(*
	       val filename = mlbdir() ## filename
*)
(* fix mael 2005-12-04: output contains something like: "basis/MLB/RI/file.o" *)
(**	       val filename = OS.Path.dir (Flags.get_string_entry "output") ## filename **)

(*	       val filename = OS.Path.mkAbsolute{path=filename,relativeTo=OS.Path.concat(OS.FileSys.getDir(), pmdir())} *)
	       type pos = ElabInfo.ParseInfo.SourceInfo.pos
	       fun info_to_positions (i : ElabInfo.ElabInfo) : pos * pos =
		 (ElabInfo.ParseInfo.SourceInfo.to_positions o 
		  ElabInfo.ParseInfo.to_SourceInfo o 
		  ElabInfo.to_ParseInfo) i
	       val info = TopdecGrammar.info_on_strexp strexp
	       val (left, right) = info_to_positions info

               val filetext = 
                   ("structure " ^ funid_string ^ " ") ^
                   LexBasics.get_source {left=left,right=right}
(*                              
	       val os = TextIO.openOut filename
	       val _ = TextIO.output(os, "structure " ^ funid_string ^ " ")
	       val _ = LexBasics.output_source{os=os,left=left,right=right}
	       val _ = TextIO.closeOut os
               val filetext = withFile TextIO.inputAll filename
*)

	       (* Explicit environment for the closure (function) below *)
	       val BBC : BodyBuilderClos = 
		   {infB=infB,elabB=elabB,absprjid=absprjid,
		    (**filename=filename,**)filemd5=MD5.fromString filetext,filetext=filetext,
                    opaq_env=opaq_env,T=T,resE=resE}
	   in BBC
	   end handle (X as IO.Io {name=s,...}) => 
	       (print ("Error while blasting out functor body for " ^ 
		       (FunId.pr_FunId funid) ^ "\n" ^ s);
		raise X)

    fun int_funbind (absprjid: absprjid, intB: IntBasis, 
		     FUNBIND(i, funid, strid, sigexp, strexp, funbind_opt)) 
	: IntFunEnv =
      let val {funids,longtycons,longstrids,longvids,sigids} = FreeIds.fid_strexp_sigexp strid strexp sigexp
	  val intB0 = IntBasis.restrict(intB, {funids=funids,sigids=sigids,
					       longstrids=longstrids,
					       longvids=longvids,longtycons=longtycons}, 
					TyName.Set.empty)
	  val funstamp = FunStamp.new funid
	  val (E, body_builder_info) =
	    case to_TypeInfo i
	      of SOME (ElabInfo.TypeInfo.FUNBIND_INFO {argE,elabBref=ref elabB,T,resE,opaq_env_opt=SOME opaq_env}) => 
		let (*val _ = print_basis elabB*)
		    val infB = case (ElabInfo.ParseInfo.to_DFInfo o ElabInfo.to_ParseInfo) i
				 of SOME (ElabInfo.ParseInfo.DFInfo.INFIX_BASIS infB) => infB
				  | _ => die "int_funbind.no infix basis info" 
		in (argE, {infB=infB,elabB=elabB,T=TyName.Set.list T,resE=resE,opaq_env=opaq_env})    (* NOTE: elabB is restricted when running FreeIds on program unit! *)
		end
	       | _ => die "int_funbind.no type info"
	  val BBC = generate_BBC(absprjid, funid, strid, body_builder_info, strexp)
	  val fe = IntFunEnv.add(funid,(absprjid,funstamp,strid,E,BBC,intB0),IntFunEnv.empty)
(*
	  val _ = print ("Closure interpretation basis for " ^ FunId.pr_FunId funid ^ " :\n")
	  val _ = print_intbasis intB0
*)
      in case funbind_opt
	   of SOME funbind => 
	     let val fe' = int_funbind(absprjid, intB, funbind)
	     in IntFunEnv.plus(fe,fe')
	     end
	    | NONE => fe
      end

    fun int_sigbind(SIGBIND (i, sigid, _, sigbind_opt)) =
      let val T = case to_TypeInfo i
		    of SOME(ElabInfo.TypeInfo.SIGBIND_INFO T) => T
		     | _ => die "int_sigbind; no type info"
	  val ise = IntSigEnv.add(sigid,T,IntSigEnv.empty)
      in case int_sigbind_opt sigbind_opt
	   of SOME ise' => IntSigEnv.plus(ise,ise')
	    | NONE => ise
      end

    and int_sigbind_opt NONE = NONE
      | int_sigbind_opt (SOME sigbind) = SOME(int_sigbind sigbind)

    fun int_sigdec (SIGNATUREsigdec (_,sigbind)) = int_sigbind sigbind

    datatype IntBasisTree = LEAF of IntBasis
                          | PLUS of IntBasisTree  * IntBasisTree

    fun flatten(LEAF e,acc) = e:: acc
      | flatten(PLUS(e1,e2), acc) = flatten(e1, flatten(e2,acc))

    fun int_topdec (fi:bool,unitnameOpt:string option, absprjid: absprjid, intB: IntBasis, topdec: topdec) 
	: IntBasisTree * modcode =
      case topdec
	of STRtopdec(i,strdec,topdec_opt) =>
	  let val (ce,cb,modc1) = comp_int_strdec(fi,intB,strdec,unitnameOpt)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce,cb)
	  in case topdec_opt
	       of SOME topdec2 =>
		 let val (intB2,modc2) = int_topdec(fi,NONE,absprjid,IntBasis.plus(intB,intB1),topdec2)
		 in (PLUS(LEAF intB1,intB2), ModCode.seq(modc1,modc2))
		 end
		| NONE => (LEAF intB1,modc1)
	  end
	 | SIGtopdec(i, sigdec, topdec_opt) =>
	  let val ise = int_sigdec(sigdec)      (* collect tynames *)
	      val intB1 = IntBasis.mk(IntFunEnv.empty, ise, CE.emptyCEnv, CompileBasis.empty)
	  in case topdec_opt
	       of SOME topdec2 =>
		 let val (intB2,modc2) = int_topdec(fi,unitnameOpt,absprjid,IntBasis.plus(intB,intB1),topdec2)
		 in (PLUS(LEAF intB1,intB2), modc2)
		 end
		| NONE => (LEAF intB1,ModCode.empty)
	  end
	 | FUNtopdec(i,FUNCTORfundec (i', funbind),topdec_opt) => 
	  let val fe = int_funbind(absprjid, intB, funbind)
	      val intB1 = IntBasis.mk(fe, IntSigEnv.empty, CE.emptyCEnv, CompileBasis.empty)
	  in case topdec_opt
	       of SOME topdec2 =>
		 let val (intB2,modc2) = int_topdec(fi,unitnameOpt,absprjid,IntBasis.plus(intB,intB1),topdec2)
		 in (PLUS(LEAF intB1,intB2), modc2)
		 end
		| NONE => (LEAF intB1,ModCode.empty)
	  end

    (* ----------------------------------------------------
     * Compile a topdec by first compiling the topdec into
     * a sequence of strdecs. In the process functors are
     * eliminated by inlining. After this compilation, the
     * resulting strdecs are compiled as usual. 
     * ---------------------------------------------------- *)

    (* ----------------------------------------------------
     * The main interpretation function
     * ---------------------------------------------------- *)

    (* fi:bool specifies whether functor applications in the topdec
     * should be inlined. *)

    fun interp_aux(fi:bool,absprjid,intB,topdec, unitname) =
      case push_topdec topdec
	of (SOME strdec, topdec_opt) => 
        	  let val (ce1,cb1,mc1) = comp_int_strdec(fi,intB,strdec, SOME unitname)
	              val intB1 = IntBasis.mk(IntFunEnv.empty,IntSigEnv.empty,ce1,cb1)
          	  in case topdec_opt
	             of SOME topdec => 
        		 let val (intB2, mc2) = int_topdec(fi,NONE,absprjid,IntBasis.plus(intB,intB1), topdec)
           		 in (PLUS(LEAF intB1,intB2)(*IntBasis.plus(intB1,intB2)*), ModCode.seq(mc1,mc2))
		         end
         		| NONE => (LEAF intB1, mc1)
	           end 
	 | (NONE, NONE) => (LEAF IntBasis.empty, ModCode.empty)
	 | (NONE, SOME topdec) => int_topdec(fi,SOME unitname,absprjid,intB,topdec)

    fun interp(fi:bool,absprjid,intB,topdec, unitname) =
      let 
	val _ = Execution.preHook()
        val (t, mc) = interp_aux(fi,absprjid,intB,topdec, unitname)
	val MLB_slash_unitname = 
	    let val {dir,file} = OS.Path.splitDirFile unitname
	    in dir ## mlbdir() ## file handle _ => die "concat"
	    end
	val _ = Execution.postHook {unitname=MLB_slash_unitname} handle _ => die "postHook"
        val intBs = flatten(t,[])
      in
        (List.foldl (fn(x, acc)=> IntBasis.plus(acc,x)) IntBasis.empty intBs, mc)
      end

  end



