
functor Manager(structure ManagerObjects : MANAGER_OBJECTS
		structure Name : NAME
		  sharing type Name.name = ManagerObjects.name
		structure ModuleEnvironments : MODULE_ENVIRONMENTS
		  sharing type ModuleEnvironments.Basis = ManagerObjects.ElabBasis
		structure Environments : ENVIRONMENTS
		  sharing type Environments.Env = ManagerObjects.ElabEnv = ModuleEnvironments.Env
		structure ParseElab : PARSE_ELAB
		  sharing type ParseElab.InfixBasis = ManagerObjects.InfixBasis
		      and type ParseElab.ElabBasis = ManagerObjects.ElabBasis
	        structure IntModules : INT_MODULES
		  sharing type IntModules.IntBasis = ManagerObjects.IntBasis
		      and type IntModules.topdec = ParseElab.topdec
		      and type IntModules.modcode = ManagerObjects.modcode
		structure FreeIds : FREE_IDS
		  sharing type FreeIds.topdec = ParseElab.topdec
		      and type FreeIds.id = ManagerObjects.id = ModuleEnvironments.id
		      and type FreeIds.tycon = ManagerObjects.tycon = ModuleEnvironments.tycon
		      and type FreeIds.strid = ManagerObjects.strid = ModuleEnvironments.strid
		      and type FreeIds.funid = ManagerObjects.funid = ModuleEnvironments.funid
		      and type FreeIds.sigid = ManagerObjects.sigid = ModuleEnvironments.sigid
		structure OpacityElim : OPACITY_ELIM
		  sharing OpacityElim.TyName = Environments.TyName = ManagerObjects.TyName
		    = ModuleEnvironments.TyName
		      and type OpacityElim.topdec = ParseElab.topdec
		      and type OpacityElim.realisation = ManagerObjects.realisation
	        structure Timing : TIMING
		structure Crash : CRASH
		structure Report : REPORT
		  sharing type Report.Report = ParseElab.Report
		structure PP : PRETTYPRINT
		  sharing type PP.StringTree = FreeIds.StringTree = ManagerObjects.StringTree
                structure Flags : FLAGS) : MANAGER =
  struct

    structure Int = Edlib.Int
    structure List = Edlib.List
    structure String = Edlib.String
    structure StringParse = Edlib.StringParse
    structure EqSet = Edlib.EqSet

    structure Basis = ManagerObjects.Basis
    structure FunStamp = ManagerObjects.FunStamp
    structure ModCode = ManagerObjects.ModCode
    structure Repository = ManagerObjects.Repository
    structure IntBasis = ManagerObjects.IntBasis
    structure ElabBasis = ModuleEnvironments.B
    structure ErrorCode = ParseElab.ErrorCode

    fun die s = Crash.impossible ("Manager." ^ s)
    fun error (s : string) = (print "\nError: "; print s; print "\n\n")


    (* -----------------------------------------
     * Unit names, file names and directories
     * ----------------------------------------- *)

    type filename = ManagerObjects.filename       (* At some point we should use *)
     and filepath = ManagerObjects.filepath       (* abstract types for these things *)
     and funid = ManagerObjects.funid             (* so that we correctly distinguish *)
     and funstamp = ManagerObjects.funstamp       (* unit names and file names. *)

    val source_directory = Flags.lookup_string_entry "source_directory"
    val log_directory = Flags.lookup_string_entry "log_directory"
    fun unitname_to_logfile unitname = !log_directory ^ unitname ^ ".log"
    fun unitname_to_sourcefile unitname = !source_directory ^ unitname ^ ".sml"
    fun filename_to_unitname (f:string) : string =
      case rev (explode f)
	of #"l":: #"m":: #"s":: #"."::unitname => implode (rev unitname)
	 | _ => die "filename_to_unitname.filename not ending with .sml"

    val log_to_file = Flags.lookup_flag_entry "log_to_file"


    (* ----------------------------------------------------
     * log_init  gives you back a function for cleaning up
     * ---------------------------------------------------- *)

    fun log_init unitname =
      let val old_log_stream = !Flags.log
	  val log_file = unitname_to_logfile unitname
	  val source_file = unitname_to_sourcefile unitname
      in if !log_to_file then
	   let val log_stream = TextIO.openOut log_file
	             handle IO.Io {name=msg,...} => 
		       die ("Cannot open log file\n\
			    \(non-exsisting directory or write-\
			    \protected existing log file?)\n" ^ msg)
	       fun log_init() = (Flags.log := log_stream;
				 TextIO.output (log_stream, "\n\n********** "
					 ^ source_file ^ " *************\n\n"))
	       fun log_cleanup() = (Flags.log := old_log_stream; TextIO.closeOut log_stream;
				    TextIO.output (TextIO.stdOut, "[wrote log file:\t" ^ log_file ^ "]\n"))
	   in log_init();
	      log_cleanup
	   end
	 else 
	   let val log_stream = TextIO.stdOut
	       fun log_init() = Flags.log := log_stream
	       fun log_cleanup() = Flags.log := old_log_stream
	   in log_init();
	      log_cleanup
	   end
      end

    fun log (s:string) : unit = TextIO.output (!Flags.log, s)
    fun log' s = log (s ^ "\n")
    fun log_st (st) : unit = PP.outputTree (log, st, 70)
    fun chat s = if !Flags.chat then log s else ()
	  
    (* ----------------------------------------
     * Some parsing functions
     * ---------------------------------------- *)

    exception ParseProjectFile of string
    fun drop_comments (l: string list) : string list =
      let fun loop(n, "(" :: "*" :: rest ) = loop(n+1, rest)
	    | loop(n, "*" :: ")" :: rest ) = loop(n-1, if n=1 then " "::rest else rest)
	    | loop(0, ch ::rest) = ch :: loop (0,rest)
	    | loop(0, []) = []
	    | loop(n, ch ::rest) = loop(n,rest)
	    | loop(n, []) = raise ParseProjectFile "unclosed comment"
      in loop(0,l)
      end
	
    (* ------------------------------------------- 
     * Debugging and reporting
     * ------------------------------------------- *)

    fun debug_free_ids ids =
      (log ("\nFree ids:");
       log_st (FreeIds.layout_ids ids);
       log "\n")

    fun print_error_report report = Report.print' report (!Flags.log)
    fun print_result_report report = (Report.print' report (TextIO.stdOut(*!Flags.log*));
				      Flags.report_warnings ())

    (* ---------------------------------------
     * Reset and commit
     * --------------------------------------- *)
	
    fun reset() = (IntModules.reset(); Repository.clear(); Flags.reset_warnings())
    fun commit() = IntModules.commit()


    (* ----------
     * Projects
     * ---------- *)

    type punit = funid * filepath
    type project = punit list

    val project : project ref = ref []


    (* ----------------------------
     * show ()  show the project
     * ---------------------------- *)

    fun show () : unit =
      let
	  val sz =
	    let val unitnames = map (filename_to_unitname o ManagerObjects.funid_to_filename o #1) (!project)
	        fun max(a:int,b) = if a > b then a else b
		fun sz [] m = m
		  | sz (s::rest) m = sz rest (max(size s,m))
	    in 2 + sz unitnames 0
	    end
	  fun pr [] = ()
	    | pr ((funid, path:string)::rest) = 
	    let val unitname = (filename_to_unitname o ManagerObjects.funid_to_filename) funid
	    in print " "; print (String.padR " " (sz - size unitname) unitname); 
	       print path; print "\n"; pr rest
	    end
      in print (String.padR "-" sz "" ^ "---------\n"); 
	 pr (!project);
	 print (String.padR "-" sz "" ^ "---------\n")
      end

    (* ----------------------------------------------------------
     * read(name)  (re)reads project file `name' into project. The
     *             repository is not reset; use reset() for this.
     * ---------------------------------------------------------- *)

    exception RepeatedProgramUnit of string
    fun read (s : string) : unit =  
      let val s = !source_directory ^ s
 	  fun is_whitesp "\n" = true
	    | is_whitesp " " = true
	    | is_whitesp "\t" = true
	    | is_whitesp _ = false
	    
	  fun lex_whitesp (all as c::rest) = if is_whitesp c then lex_whitesp rest
					     else all 
	    | lex_whitesp [] = []
	  fun lex_name(c::rest, acc) = if is_whitesp c then (concat(rev acc), rest)
				       else lex_name (rest, c::acc)
	    | lex_name ([], acc) = (concat(rev acc), [])
	  fun lex_names acc cs = case lex_whitesp cs
				   of [] => rev acc
				    | cs => let val (name,cs') = lex_name(cs, [])
					    in if List.member name acc then raise RepeatedProgramUnit name
					       else lex_names (name::acc) cs'
					    end

	  val unitnames = (lex_names [] o drop_comments o (map str) o explode o StringParse.fromFile) s

	  (* For the repository not to be messed up it is important to
	   * keep functor identifiers stemming from user declarations
	   * distinct from functor identifiers stemming from file
	   * names. This distinction is guarantied by the `.sml' file
	   * extension on filenames.
	   *)

	  val filenames = map (fn f => f ^ ".sml") unitnames
      in
	 project := map (fn f => (ManagerObjects.funid_from_filename f,
				  !source_directory ^ f)) filenames
      end handle IO.Io {name=io_s,...} => 
	           error ("Project file `" ^ s ^ "' cannot be opened.")
               | ParseProjectFile s1 => 
	           error ("Parsing project file `" ^ s ^ "': " ^ s1)
	       | RepeatedProgramUnit name => 
	           error ("Repeated program unit `" ^ name ^ "' in project file `" ^ s ^ "'.")


    type Basis = ManagerObjects.Basis
    type modcode = ManagerObjects.modcode

    (* Matching of export elaboration and interpretation bases to
     * those in repository for a given funid *)

    fun match_elab(names_elab, elabB, funid) =
      case Repository.lookup_elab funid
	of SOME (_,(_,_,_,names_elab',_,elabB',_)) =>    (* names_elab' are already marked generative - lookup *)
	  (List.apply Name.mark_gen names_elab;          (* returned the entry. The invariant is that every *)
	   ElabBasis.match(elabB, elabB');               (* name in the bucket is generative. *)
	   List.apply Name.unmark_gen names_elab)
	 | NONE => () (*bad luck*)

    fun match_int(names_int, intB, funid) =
      case Repository.lookup_int funid
	of SOME(_,(_,_,_,names_int',_,intB')) =>     (* names_int' are already marked generative - lookup *)
	  (List.apply Name.mark_gen names_int;     (* returned the entry. The invariant is that every *)
	   IntBasis.match(intB, intB');            (* name in the bucket is generative. *)
	   List.apply Name.unmark_gen names_int)
	 | NONE => () (*bad luck*)

    (* --------------------------------
     * Parse, elaborate and interpret
     * (may raise PARSE_ELAB_ERROR)
     * -------------------------------- *)

    fun free_ids a = FreeIds.free_ids a
    fun ElabBasis_restrict a = ElabBasis.restrict a
    fun IntBasis_restrict a = IntBasis.restrict a
    fun OpacityElim_restrict a = OpacityElim.restrict a
    fun opacity_elimination a = OpacityElim.opacity_elimination a

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list
    fun parse_elab_interp (B, funid, source_filepath, funstamp_now) : Basis * modcode =
          let val _ = Timing.reset_timings()
	      val _ = Timing.new_file(source_filepath)
	      val (infB, elabB, rea, intB) = Basis.un B
	      val unitname = (filename_to_unitname o ManagerObjects.funid_to_filename) funid
	      val log_cleanup = log_init unitname
	      val _ = Name.bucket := []
	      val _ = Flags.reset_warnings ()
	      (* val _ = print "\n[parsing and elaborating ...\n" *)
	      val res = ParseElab.parse_elab {infB=infB,elabB=elabB, file=source_filepath} 
	      (* val _ = print " done]\n" *)
	  in (case res
		of ParseElab.FAILURE (report, error_codes) => (print_error_report report; raise PARSE_ELAB_ERROR error_codes)
		 | ParseElab.SUCCESS {report,infB=infB',elabB=elabB',topdec} =>
		  let val names_elab = !Name.bucket

		      val _ = chat "[finding free identifiers begin...]\n"
		      val freeids as {ids,tycons,strids,funids,sigids} =
			let val ids = free_ids topdec
			in {ids=FreeIds.vids_of_ids ids, tycons=FreeIds.tycons_of_ids ids,
			    strids=FreeIds.strids_of_ids ids, funids=FreeIds.funids_of_ids ids,
			    sigids=FreeIds.sigids_of_ids ids}
			end
		      val _ = chat "[finding free identifiers end...]\n"

		      (* val _ = debug_free_ids ids *)
		      val _ = chat "[restricting elaboration basis begin...]\n"
		      val elabB_im = ElabBasis_restrict(elabB,freeids)
		      val _ = chat "[restricting elaboration basis end...]\n"
		      (* val _ = debug_basis "Import" Bimp *)

		      val _ = chat "[restricting interpretation basis begin...]\n"
		      val intB_im = IntBasis_restrict(intB, (funids,strids,ids,tycons))
		      val _ = chat "[restricting interpretation basis end...]\n"

 		      val tynames_elabB_im = ElabBasis.tynames elabB_im
		      val rea_im = OpacityElim_restrict(rea,tynames_elabB_im)

		      val _ = chat "[opacity elimination begin...]\n"
		      val (topdec', rea') = opacity_elimination(rea_im, topdec)
		      val _ = chat "[opacity elimination end...]\n"

		      val _ = chat "[interpretation begin...]\n"
		      val _ = Name.bucket := []
		      val (intB', modc) = IntModules.interp(intB_im, topdec', unitname)
		      val names_int = !Name.bucket
		      val _ = Name.bucket := []
		      val _ = chat "[interpretation end...]\n"

		      (* match export elaboration and interpretation
		       * bases to those found in repository. *)

		      val _ = chat "[matching begin...]\n"
		      val _ = match_elab(names_elab, elabB', funid)
		      val _ = match_int(names_int, intB', funid)
		      val _ = chat "[matching end...]\n"

		      val _ = Repository.delete_entries funid

		      val _ = Repository.add_elab (funid, (infB, elabB_im, (rea_im,tynames_elabB_im), 
							   names_elab, infB', elabB', rea'))
		      val modc = ModCode.emit modc  (* When module code is inserted in repository,
						     * names become rigid, so we emit the module code. *)
		      val elabE' = ElabBasis.to_E elabB'
		      val _ = Repository.add_int (funid, (funstamp_now, elabE', intB_im, names_int, modc, intB'))
		      val B' = Basis.mk(infB',elabB',rea',intB')
		  in print_result_report report;
		    log_cleanup();
		    (B',modc)
		  end handle ? => (print_result_report report; log_cleanup(); raise ?)
		) handle XX => (log_cleanup(); raise XX)
	  end  

    (* ----------------
     * build a unit
     * ---------------- *)

    fun Repository_lookup_elab a = Repository.lookup_elab a
    fun Repository_lookup_int a = Repository.lookup_int a
    fun Basis_enrich a = Basis.enrich a

    fun build_unit(B, (funid, filepath): punit) : Basis * modcode =
      let val funstamp_now = FunStamp.from_filemodtime filepath  (*always get funstamp before reading content*)
	  exception CAN'T_REUSE
      in (case (Repository_lookup_elab funid, Repository_lookup_int funid)
	    of (SOME(_,(infB, elabB, (rea,dom_rea), names_elab,infB',elabB', rea')), 
		SOME(_,(funstamp,elabE,intB,names_int,modc,intB'))) =>
	      let val B_im = Basis.mk(infB,elabB,rea,intB)
	      in if FunStamp.eq(funstamp,funstamp_now) andalso
		     let (* val _ = print "\n[checking enrichment ...\n" *)
		         fun unmark_names () = (List.apply Name.unmark_gen names_elab;  (* Unmark names - they where *)
						List.apply Name.unmark_gen names_int)   (* marked in the repository. *)
			 fun remark_names () = (List.apply Name.mark_gen names_elab;    (*  If enrichment fails we remark *)
						List.apply Name.mark_gen names_int)     (* names; notice that enrichment of *)
			                                                                (* elaboration bases requires all *)
			 val _ = unmark_names()                                         (* names be unmarked. Names in the *)
		         val res = Basis_enrich(B, (B_im, dom_rea))                     (* global basis are always unmarked. *)
			 (* val _ = print " done]\n" *)
		     in (if res then () else remark_names() ; res)
		     end 

(* andalso
		     let (* val _ = print "\n[checking environment equality ...\n" *)   (* Why is this necessary? *)
		         val res = Environments.E.eq(ElabBasis.to_E elabB', elabE)    (* I've commented it out 14/10/97-Martin *)
			 (* val _ = print " done]\n" *)
		     in res
		     end 
*)              
                   then 
		    let val _ = print ("[reusing code for: \t" ^ filepath ^ "]\n")
		        val B_ex = Basis.mk(infB',elabB',rea',intB')
		    in 
		       (B_ex, modc)
		    end
		 else raise CAN'T_REUSE
	      end
	     | _ =>  raise CAN'T_REUSE)
	handle CAN'T_REUSE =>
	  parse_elab_interp (B, funid, filepath, funstamp_now)
      end 

    (* ----------------
     * build a project
     * ---------------- *)

    fun Basis_plus (B,B') = Basis.plus(B,B')		    

    fun build_proj(B: Basis, project: project) : modcode =
      case project
	of [] => ModCode.empty
	 | (punit::project') => 
	  let val (B', modc) = build_unit(B, punit)
	      (* val _ = print "\n[adding result to basis ...\n" *)
	      val B'' = Basis_plus(B,B')
	      (* val _ = print " done]\n" *)
	      val modc' = build_proj(B'', project')
	  in ModCode.seq(modc,modc')
	  end

    (* ------------------------------------
     * build()  builds a loaded project
     * ------------------------------------ *)
			
    fun build() =   (* May raise PARSE_ELAB_ERROR *)
      let val _ = Repository.recover()
	  val emitted_files = EqSet.fromList (Repository.emitted_files())
	  val _ = let val modc = build_proj(Basis.initial, !project)
		  in ModCode.mk_exe (modc, "run")
		  end
	  val emitted_files' = EqSet.fromList (Repository.emitted_files())
    	  val files_to_delete = EqSet.list (EqSet.difference emitted_files emitted_files')
      in List.apply ManagerObjects.SystemTools.delete_file files_to_delete
      end

    (* -----------------------------
     * Compile a single file 
     * ----------------------------- *)

    fun comp (unitname : string) : unit =
      let val (infB,elabB,rea,intB) = Basis.un Basis.initial
	  val _ = reset()
	  val _ = Timing.reset_timings()
	  val _ = Timing.new_file unitname
	  val log_cleanup = log_init unitname
      in (case ParseElab.parse_elab {infB=infB,elabB=elabB,
				     file=unitname_to_sourcefile unitname} 
	    of ParseElab.SUCCESS {report, topdec, ...} =>
	      let val (topdec',_) = OpacityElim.opacity_elimination(rea,topdec)
		  val (_, modc) = IntModules.interp(intB, topdec', unitname)
		  val modc = ModCode.emit modc
	      in ModCode.mk_exe(modc, unitname ^ ".exe");
	         print_result_report report;
		 log_cleanup()
	      end
	     | ParseElab.FAILURE (report, error_codes) => (print_error_report report; raise PARSE_ELAB_ERROR error_codes)
         ) handle XX => (log_cleanup(); raise XX)
      end 


    (* -----------------------------
     * Elaborate a single file 
     * ----------------------------- *)

    fun elab (unitname : string) : unit =
      let val (infB,elabB,_,_) = Basis.un Basis.initial
	  val _ = reset()
	  val log_cleanup = log_init unitname
      in (case ParseElab.parse_elab {infB=infB,elabB=elabB,
				     file=unitname_to_sourcefile unitname} 
	    of ParseElab.SUCCESS {report, ...} => (print_result_report report; log_cleanup())
	     | ParseElab.FAILURE (report, error_codes) => (print_error_report report; raise PARSE_ELAB_ERROR error_codes)
	 ) handle E => (log_cleanup(); raise E)
      end 


    (* initialize Flags.build_ref to contain build (for interaction), etc.
     * See comment in FLAGS.*)

    fun wrap f a = (f a) handle PARSE_ELAB_ERROR _ => 
      TextIO.output(TextIO.stdOut, "\n ** Parse or elaboration error occurred. **\n")

    val _ = Flags.build_project_ref := wrap build
    val _ = Flags.show_project_ref := show
    val _ = Flags.read_project_ref := read
    val _ = Flags.comp_ref := wrap comp

    val interact = Flags.interact
    val read_script = Flags.read_script

  end
