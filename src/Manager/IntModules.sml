(*$IntModules: NAME MANAGER_OBJECTS COMPILER_ENV ELAB_INFO
               ENVIRONMENTS COMPILE_BASIS COMPILE TOPDEC_GRAMMAR
               FREE_IDS CRASH FLAGS INT_MODULES*)

functor IntModules(structure Name : NAME
		   structure ManagerObjects : MANAGER_OBJECTS
		     sharing type ManagerObjects.name = Name.name
		   structure CompilerEnv : COMPILER_ENV
		     sharing type CompilerEnv.CEnv = ManagerObjects.CEnv
		   structure ElabInfo : ELAB_INFO
		     sharing type ElabInfo.TypeInfo.Env = CompilerEnv.ElabEnv 
		                                        = ManagerObjects.ElabEnv
		   structure Environments : ENVIRONMENTS
		     sharing type Environments.realisation = ElabInfo.TypeInfo.realisation
		         and type Environments.Env = CompilerEnv.ElabEnv
		   structure CompileBasis : COMPILE_BASIS
		     sharing type CompileBasis.CompileBasis = ManagerObjects.CompileBasis 
		   structure Compile : COMPILE
		     sharing type Compile.CEnv = ManagerObjects.CEnv
		         and type Compile.CompileBasis = ManagerObjects.CompileBasis
			 and type Compile.linkinfo = ManagerObjects.linkinfo
			 and type Compile.target = ManagerObjects.target
		   structure TopdecGrammar : TOPDEC_GRAMMAR
		     sharing type TopdecGrammar.strdec = Compile.strdec
		         and type TopdecGrammar.strid = CompilerEnv.strid
			 and type TopdecGrammar.info = ElabInfo.ElabInfo
		   structure FreeIds : FREE_IDS
		     sharing type FreeIds.id = ManagerObjects.id
		         and type FreeIds.strid = ManagerObjects.strid = TopdecGrammar.strid
		         and type FreeIds.funid = ManagerObjects.funid = TopdecGrammar.funid
			 and type FreeIds.strexp = TopdecGrammar.strexp = ManagerObjects.strexp
		   structure Crash : CRASH
		   structure Flags: FLAGS
		   ) : INT_MODULES =
  struct
    structure FunStamp = ManagerObjects.FunStamp
    structure Repository = ManagerObjects.Repository
    structure IntBasis = ManagerObjects.IntBasis
    structure IntFunEnv = ManagerObjects.IntFunEnv
    structure ModCode = ManagerObjects.ModCode
    structure CE = CompilerEnv

    fun die s = Crash.impossible ("IntModules." ^ s)

    type IntBasis = ManagerObjects.IntBasis
     and topdec = TopdecGrammar.topdec
     and modcode = ManagerObjects.modcode
     and CompileBasis = CompileBasis. CompileBasis
     and CEnv = CE.CEnv
     and IntFunEnv = ManagerObjects.IntFunEnv

    open TopdecGrammar (*declares StrId*)


    (* ----------------------------------------------------
     * Fresh unit names; in case functors `splits' sources
     * ---------------------------------------------------- *)

    local val counter = ref 0
          fun inc() = (counter := !counter + 1; !counter)
    in fun fresh_unitname() = "code" ^ Int.string(inc())
       fun reset_unitname_counter() = counter := 0
    end


    (* ----------------------------------------------------
     * Compile a sequence of structure declarations
     * ---------------------------------------------------- *)

    fun compile_strdecs(intB, strdecs, unitname_opt) =
      let val (_,ce,cb) = IntBasis.un intB
	  val unitname = case unitname_opt
			   of Some unitname => unitname
			    | None => fresh_unitname()
	  val vcg_filename = !Flags.target_directory ^ unitname ^ ".vcg"
      in case Compile.compile(ce,cb,strdecs,vcg_filename)
	   of Compile.CodeRes(ce',cb',target,linkinfo) =>
	     (ce',cb', ModCode.mk_modcode(target,linkinfo,unitname))
	    | Compile.CEnvOnlyRes ce' => (ce', CompileBasis.empty, ModCode.empty)
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
				     of Some strbind => funapp_strbind strbind
				      | None => false)
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

    fun push_topdec(SIGtopdec(i,sigdec, topdec_opt)) = push_topdec_opt topdec_opt
      | push_topdec(STRtopdec(i,strdec, topdec_opt)) =
        (case push_topdec_opt topdec_opt
	   of (Some strdec', topdec_opt') => (Some(SEQstrdec(i,strdec,strdec')), topdec_opt')
	    | (None, topdec_opt') => (Some strdec, topdec_opt'))
      | push_topdec(FUNtopdec(i,fundec, topdec_opt)) =
	   let val topdec_opt' = 
	         case push_topdec_opt topdec_opt
		   of (Some strdec, topdec_opt') => Some(STRtopdec(i,strdec,topdec_opt'))
		    | (None, topdec_opt') => topdec_opt'
	   in (None, Some (FUNtopdec(i,fundec, topdec_opt')))
	   end 
    and push_topdec_opt (Some topdec) = push_topdec topdec
      | push_topdec_opt None = (None, None)

    (* strdecs_of_strdec strdec  eliminates SEQstrdec and 
     * EMPTYstrdec at upper level *)

    fun strdecs_of_strdec (strdec, a) =
      case strdec
	of EMPTYstrdec _ => a
	 | SEQstrdec(_,strdec1, strdec2) => strdecs_of_strdec(strdec1, strdecs_of_strdec(strdec2,a))
	 | _ => strdec :: a

    (* comp_int_strdec(intB, strdec): strdecs not containing functor
     * applications are sent to the compiler. *)

    fun comp_int_strdec (intB, strdec, unitname_opt) =
      let fun loop (intB, [], strdecs) = compile_strdecs(intB, rev strdecs, unitname_opt)
	    | loop (intB, strdec::strdecs, strdecs') =
	    if funapp_strdec strdec then
	      let val (ce1,cb1,mc1) = compile_strdecs(intB, rev strdecs', None)
		  val intB1 = IntBasis.mk(IntFunEnv.empty,ce1,cb1)
		  val intB' = IntBasis.plus(intB,intB1)
		  val (ce2,cb2,mc2) = int_strdec(intB', strdec)
		  val intB2 = IntBasis.mk(IntFunEnv.empty,ce2,cb2) 
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
	  let val (ce1,cb1,mc1) = comp_int_strdec(intB,strdec1,None)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,ce1,cb1)
	      val (ce2,cb2,mc2) = comp_int_strdec(IntBasis.plus(intB,intB1),strdec2,None)
	  in (ce2, CompileBasis.plus(cb1,cb2), ModCode.seq(mc1,mc2))
	  end
	 | DECstrdec _ => die "int_strdec.DECstrdec.the compiler should deal with this"
	 | EMPTYstrdec _ => die "int_strdec.EMPTYstrdec.linearization failed"
	 | SEQstrdec _ => die "int_strdec.SEQstrdec.linearization failed"

    and int_strbind (intB, STRBIND(i, strid, strexp, strbind_opt)) =
      let val (ce,cb,mc) = int_strexp(intB, strexp)
	  val ce = CE.declare_strid(strid,ce,CE.emptyCEnv)
      in case strbind_opt
	   of Some strbind' =>
	     let val (ce',cb',mc') = int_strbind(intB, strbind')
	     in (CE.plus(ce,ce'), CompileBasis.plus(cb,cb'), ModCode.seq(mc,mc'))
	     end
	    | None => (ce,cb,mc)
      end 

    and int_strexp (intB: IntBasis, strexp: strexp) : CEnv * CompileBasis * modcode =
      case strexp
	of STRUCTstrexp(i, strdec) => comp_int_strdec(intB, strdec,None)
	 | LONGSTRIDstrexp(i, longstrid) =>
	  let val (_,ce,_) = IntBasis.un intB
	      val (strids,strid) = StrId.explode_longstrid longstrid
	      fun lookup (ce, []) = CE.lookup_strid ce strid
		| lookup (ce, strid::strids) = lookup(CE.lookup_strid ce strid, strids)
	      val ce = lookup(ce, strids)
	  in (ce, CompileBasis.empty, ModCode.empty)
	  end
	| TRANSPARENT_CONSTRAINTstrexp(i, strexp, sigexp) =>
	  let val (ce, cb, mc) = int_strexp(intB, strexp)
	      val E = case ElabInfo.to_TypeInfo i
			of Some (ElabInfo.TypeInfo.TRANS_CONSTRAINT_INFO E) => E
			 | _ => die "int_strexp.TRANSPARENT_CONSTRAINTstrexp.no env info"
	      val ce' = CE.constrain(ce,E)
	  in (ce', cb, mc)
	  end
	| OPAQUE_CONSTRAINTstrexp(i, strexp, sigexp) => die "OPAQUE_CONSTRAINTstrexp.not impl"
	| APPstrexp(i, funid, strexp) => 
	  let val phi = case ElabInfo.to_TypeInfo i
			  of Some (ElabInfo.TypeInfo.FUNCTOR_APP_INFO phi) => phi
			   | _ => die "int_strexp.APPstrexp.no phi info"
	      val (ce, cb, mc) = int_strexp(intB, strexp)
	      val (funstamp,strid,E,strexp0,intB0) = IntFunEnv.lookup ((#1 o IntBasis.un) intB) funid
	      val E' = Environments.Realisation.on_Env phi E
	      val ce = CE.constrain(ce,E')
	      val intB1 = IntBasis.mk(IntFunEnv.empty,CE.declare_strid(strid,ce,CE.emptyCEnv),
				      CompileBasis.plus(#3 (IntBasis.un intB), cb))

	      (* We now check if there is code in the repository
	       * we can reuse. *)
	      fun reuse_code () =
		case Repository.lookup_int funid
		  of Some(_,(funstamp',intB',N',mc',intB'')) =>
		    if FunStamp.eq(funstamp,funstamp') andalso
		       IntBasis.enrich(IntBasis.plus(intB0,intB1),intB') then 
		      let val _ = print ("  [reusing instance code for functor " ^ FunId.pr_FunId funid ^ "]\n")
			  val (_,ce',cb') = IntBasis.un intB''
			  val _ = List.apply Name.unmark_gen N'   (* unmark names - they where *)
		      in Some(ce',cb',mc')                        (* marked in the repository. *)
		      end
		    else None
		   | None => None 

	  in case reuse_code ()
	       of Some(ce',cb',mc') => (ce', CompileBasis.plus(cb,cb'), ModCode.seq(mc,mc'))
		| None => 
		 let val intB' = 
		       let val fid = FreeIds.free_ids_strexp strexp0
			   val (funids,strids,ids) = (FreeIds.funids_of_ids fid,
						      FreeIds.strids_of_ids fid,
						      FreeIds.vids_of_ids fid)
(*			   val _ = print "\n[doing restriction in functor app.." *)
			   val intB' = IntBasis.restrict(IntBasis.plus(intB0,intB1), (funids,strids,ids))
(*			   val _ = print "done.]\n" *)
		       in intB'
		       end
		     val strexp0' =
		       let fun on_ElabInfo(phi,i) = 
			     case ElabInfo.to_TypeInfo i
			       of Some i' => ElabInfo.plus_TypeInfo i (ElabInfo.TypeInfo.on_TypeInfo(phi,i'))
				| None => i  
		       in TopdecGrammar.map_strexp_info (fn i => on_ElabInfo(phi,i)) strexp0
		       end

		     (* Before we interpret strexp0' we force generated names
		      * to be `rigid' by emptying the generative names
		      * bucket, located in Name. This might lead to some
		      * unnecessary recompilation - but for now it'll do. -
		      * Martin *)
		     val _ = Name.bucket := []
		     val (ce',cb',mc') = int_strexp(intB', strexp0')
		     val N' = !Name.bucket
		     val _ = Name.bucket := []
		       
		     val intB'' = IntBasis.mk(IntFunEnv.empty, ce', cb')

		     val cb'' = CompileBasis.plus(cb,cb')

		     (* We now try to see if there is something in the
		      * repository we can match against. We also store the
		      * entry in the repository and we emit the generated code,
		      * since all names now have become rigid. *)
		     val mc' = case Repository.lookup_int funid
			       of Some(entry_no, (_,_,N2,_,intB2)) => (* names in N2 are already marked generative, since *)
				 (List.apply Name.mark_gen N';        (* N2 is returned by lookup_int *)
				  IntBasis.match(intB'', intB2);
				  List.apply Name.unmark_gen N';
				  let val mc' = ModCode.emit mc'
				  in Repository.owr_int(funid,entry_no,(funstamp,intB',N',mc',intB''));
				     mc'
				  end)
				| None => let val mc' = ModCode.emit mc'
					  in Repository.add_int(funid,(funstamp,intB',N',mc',intB''));
					     mc'
					  end
		     val mc'' = ModCode.seq(mc,mc')
		 in (ce', cb'', ModCode.emit mc'')     (* we also emit code for arg.. see comment above *)
		 end
	  end

	| LETstrexp(info, strdec, strexp) => 
	  let val (ce1,cb1,mc1) = comp_int_strdec(intB,strdec,None)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,ce1,cb1)
	      val (ce2,cb2,mc2) = int_strexp(IntBasis.plus(intB,intB1),strexp)
	  in (ce2, CompileBasis.plus(cb1,cb2), ModCode.seq(mc1,mc2))
	  end

    fun int_funbind (intB: IntBasis, FUNBIND(i, funid, strid, sigexp, strexp, funbind_opt)) : IntFunEnv =
      let val ids_strexp = FreeIds.free_ids_strexp strexp
	  val strids = List.dropAll (fn strid' => strid = strid') (FreeIds.strids_of_ids ids_strexp)
	  val ids = FreeIds.vids_of_ids ids_strexp
	  val funids = FreeIds.funids_of_ids ids_strexp
	  val intB0 = IntBasis.restrict(intB, (funids,strids,ids))
	  val funstamp = FunStamp.new funid
	  val E = case ElabInfo.to_TypeInfo i
		    of Some (ElabInfo.TypeInfo.FUNBIND_INFO E) => E
		     | _ => die "int_funbind.no env info"
	  val fe = IntFunEnv.add(funid,(funstamp,strid,E,strexp,intB0),IntFunEnv.empty)
      in case funbind_opt
	   of Some funbind => 
	     let val fe' = int_funbind(intB, funbind)
	     in IntFunEnv.plus(fe,fe')
	     end
	    | None => fe
      end

    fun int_topdec (intB: IntBasis, topdec: topdec) : IntBasis * modcode =   (* can effect repository *)
      case topdec
	of STRtopdec(i,strdec,topdec_opt) =>
	  let val (ce,cb,modc1) = comp_int_strdec(intB,strdec,None)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,ce,cb)
	  in case topdec_opt
	       of Some topdec2 =>
		 let val (intB2,modc2) = int_topdec(IntBasis.plus(intB,intB1),topdec2)
		 in (IntBasis.plus(intB1,intB2), ModCode.seq(modc1,modc2))
		 end
		| None => (intB1,modc1)
	  end
	 | SIGtopdec _ => die "int_topdec.SIGtopdec.should be eliminated by push_topdec"
	 | FUNtopdec(i,FUNCTORfundec (i', funbind),topdec_opt) => 
	  let val fe = int_funbind(intB, funbind)
	      val intB1 = IntBasis.mk(fe,CE.emptyCEnv, CompileBasis.empty)
	  in case topdec_opt
	       of Some topdec2 =>
		 let val (intB2,modc2) = int_topdec(IntBasis.plus(intB,intB1),topdec2)
		 in (IntBasis.plus(intB1,intB2), modc2)
		 end
		| None => (intB1,ModCode.empty)
	  end

    fun interp(intB,topdec, unitname) =
      case push_topdec topdec
	of (Some strdec, topdec_opt) => 
	  let val (ce1,cb1,mc1) = comp_int_strdec(intB,strdec, Some unitname)
	      val intB1 = IntBasis.mk(IntFunEnv.empty,ce1,cb1)
	  in case topdec_opt
	       of Some topdec => 
		 let val (intB2, mc2) = int_topdec(IntBasis.plus(intB,intB1), topdec)
		 in (IntBasis.plus(intB1,intB2), ModCode.seq(mc1,mc2))
		 end
		| None => (intB1, mc1)
	  end 
	 | (None, None) => (IntBasis.empty, ModCode.empty)
	 | (None, Some topdec) => int_topdec(intB,topdec)

    fun reset() = (Compile.reset(); reset_unitname_counter())
    val commit = Compile.commit
  end
