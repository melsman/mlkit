(*$Manager: MANAGER_OBJECTS NAME ENVIRONMENTS MODULE_ENVIRONMENTS
            PARSE_ELAB CRASH REPORT PRETTYPRINT FLAGS INT_MODULES
            FREE_IDS OPACITY_ELIM MANAGER TIMING*)

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

    structure Basis = ManagerObjects.Basis
    structure FunStamp = ManagerObjects.FunStamp
    structure ModCode = ManagerObjects.ModCode
    structure Repository = ManagerObjects.Repository
    structure IntBasis = ManagerObjects.IntBasis
    structure ElabBasis = ModuleEnvironments.B

    type filename = ManagerObjects.filename
     and filepath = ManagerObjects.filepath
     and funid = ManagerObjects.funid
     and funstamp = ManagerObjects.funstamp

    fun die s = Crash.impossible ("Manager." ^ s)
    fun error (s : string) = (print "**ERROR**\n";
			      print s; print "\n";
			      die "")

    (* ------------
     * Directories
     * ------------ *)

    val source_directory = Flags.lookup_string_entry "source_directory"
    val log_directory = Flags.lookup_string_entry "log_directory"
    fun filename_to_logfile filename = !log_directory ^ filename ^ ".log"
    fun filename_to_sourcefile file = !source_directory ^ file ^ ".sml"

    val log_to_file = Flags.lookup_flag_entry "log_to_file"

    (* ----------------------------------------------------
     * log_init  gives you back a function for cleaning up
     * ---------------------------------------------------- *)

    fun log_init filename =
      let val old_log_stream = !Flags.log
	  val log_file = filename_to_logfile filename
	  val source_file = filename_to_sourcefile filename
      in if !log_to_file then
	   let val log_stream = open_out log_file
	             handle Io msg => die ("Cannot open log file\n\
		                           \(non-exsisting directory or write-\
			                   \protected existing log file?)\n" ^ msg)
	       fun log_init() = (Flags.log := log_stream;
				 output (log_stream, "\n\n********** "
					 ^ source_file ^ " *************\n\n"))
	       fun log_cleanup() = (Flags.log := old_log_stream; close_out log_stream;
				    output (std_out, "[wrote log file:\t" ^ log_file ^ "]\n"))
	   in log_init();
	      log_cleanup
	   end
	 else 
	   let val log_stream = std_out
	       fun log_init() = Flags.log := log_stream
	       fun log_cleanup() = Flags.log := old_log_stream
	   in log_init();
	      log_cleanup
	   end
      end

    fun log (s:string) : unit = output (!Flags.log, s)
    fun log' s = log (s ^ "\n")
    fun log_st (st) : unit = PP.outputTree (log, st, 70)


    (* -----------------------------------------------------------------
     * Dynamic flags
     * ----------------------------------------------------------------- *)

    val report_file_sig = ref false
    val _ = Flags.add_flag_to_menu
          (["Control"], "report_file_sig",
	   "report types, values, etc.", report_file_sig)
	  
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

    fun reset_warnings() = Flags.warnings := []

    fun report_warnings () = 
          (case !Flags.warnings of
	     [] =>  ()
	   | [s] => (if !log_to_file then output(std_out, "\n*** 1  warning printed on log file\n")
		     else ();
		     output(!Flags.log, "*** warning: " ^ s))
	   | ss =>  (if !log_to_file then output(std_out, "\n*** " ^ Int.string (length ss)
						 ^ " warnings printed on log file\n")
		     else ();
	             List.apply (fn s => output(!Flags.log, "*** warning: " ^ s)) (rev ss)))

    fun print_error_report report = Report.print' report (!Flags.log)
    fun print_result_report report = ((if !report_file_sig then Report.print' report (!Flags.log) 
				       else ());
				      report_warnings())

    (* ---------------------------------------
     * Reset and commit
     * --------------------------------------- *)
	
    fun reset() = (IntModules.reset(); Repository.clear(); reset_warnings())
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
      let val strs = map (ManagerObjects.funid_to_filename o #1) (!project)
	  fun max(a:int,b) = if a > b then a else b
	  fun sz [] m = m
	    | sz (s::rest) m = sz rest (max(size s,m))
	  val sz = 2 + sz strs 0
	  fun pr [] = ()
	    | pr (x::xs) = (print " "; print x; print "\n"; pr xs)
      in print (String.padR "-" sz "" ^ "---------\n"); 
	 pr strs;
	 print (String.padR "-" sz "" ^ "---------\n")
      end

    (* ----------------------------------------------------------
     * read(name)  (re)reads project file `name' into project. The
     *             repository is not reset; use reset() for this.
     * ---------------------------------------------------------- *)

    fun read (s : string) : unit =  
      let val s = !source_directory ^ s
 	  fun is_whitesp "\n" = true
	    | is_whitesp " " = true
	    | is_whitesp "\t" = true
	    | is_whitesp _ = false
	    
	  fun lex_whitesp (all as c::rest) = if is_whitesp c then lex_whitesp rest
					     else all 
	    | lex_whitesp [] = []
	  fun lex_name(c::rest, acc) = if is_whitesp c then (implode(rev acc), rest)
				       else lex_name (rest, c::acc)
	    | lex_name ([], acc) = (implode(rev acc), [])
	  fun lex_names acc cs = case lex_whitesp cs
				   of [] => rev acc
				    | cs => let val (name,cs') = lex_name(cs, [])
					    in lex_names (name::acc) cs'
					    end

	  val files = (lex_names [] o drop_comments o explode o StringParse.fromFile) s
      in
	 project := map (fn f => (ManagerObjects.funid_from_filename f,
				  !source_directory ^ f ^ ".sml")) files
      end handle Io io_s => (error ("Project file \"" ^ s ^ "\" cannot be opened."))
               | ParseProjectFile s1 => (error ("Parsing project file \"" ^ s ^ "\": " ^ s1))


    type Basis = ManagerObjects.Basis
    type modcode = ManagerObjects.modcode

    (* Matching of export elaboration and interpretation bases to
     * those in repository for a given funid *)

    fun match_elab(names_elab, elabB, funid) =
      case Repository.lookup_elab funid
	of Some (_,(_,_,_,names_elab',_,elabB',_)) =>    (* names_elab' are already marked generative - lookup *)
	  (List.apply Name.mark_gen names_elab;          (* returned the entry. The invariant is that every *)
	   ElabBasis.match(elabB, elabB');               (* name in the bucket is generative. *)
	   List.apply Name.unmark_gen names_elab)
	 | None => () (*bad luck*)

    fun match_int(names_int, intB, funid) =
      case Repository.lookup_int funid
	of Some(_,(_,_,_,names_int',_,intB')) =>     (* names_int' are already marked generative - lookup *)
	  (List.apply Name.mark_gen names_int;     (* returned the entry. The invariant is that every *)
	   IntBasis.match(intB, intB');            (* name in the bucket is generative. *)
	   List.apply Name.unmark_gen names_int)
	 | None => () (*bad luck*)

    (* --------------------------------
     * Parse, elaborate and interpret
     * (may raise PARSE_ELAB_ERROR)
     * -------------------------------- *)

    exception PARSE_ELAB_ERROR
    fun parse_elab_interp (B, funid, source_filepath, funstamp_now) : Basis * modcode =
          let val _ = Timing.reset_timings()
	      val _ = Timing.new_file(source_filepath)
	      val (infB, elabB, rea, intB) = Basis.un B
	      val filename = ManagerObjects.funid_to_filename funid
	      val log_cleanup = log_init filename
	      val _ = Name.bucket := []
	      val _ = reset_warnings ()
	      (* val _ = print "\n[parsing and elaborating ...\n" *)
	      val res = ParseElab.parse_elab {infB=infB,elabB=elabB, file=source_filepath} 
	      (* val _ = print " done]\n" *)
	  in (case res
		of ParseElab.FAILURE report => (print_error_report report; raise PARSE_ELAB_ERROR)
		 | ParseElab.SUCCESS {report,infB=infB',elabB=elabB',topdec} =>
		  let val names_elab = !Name.bucket

		      val freeids as {ids,tycons,strids,funids,sigids} =
			let val ids = FreeIds.free_ids topdec
			in {ids=FreeIds.vids_of_ids ids, tycons=FreeIds.tycons_of_ids ids,
			    strids=FreeIds.strids_of_ids ids, funids=FreeIds.funids_of_ids ids,
			    sigids=FreeIds.sigids_of_ids ids}
			end

		      (* val _ = debug_free_ids ids *)
		      val elabB_im = ElabBasis.restrict(elabB,freeids)
		      (* val _ = debug_basis "Import" Bimp *)

		      (* val _ = print "\n[restricting interpretation basis ...\n" *)
		      val intB_im = IntBasis.restrict(intB, (funids,strids,ids,tycons))
		      (* val _ = print " done]\n" *)

 		      val tynames_elabB_im = ElabBasis.tynames elabB_im
		      val rea_im = OpacityElim.restrict(rea,tynames_elabB_im)

		      val (topdec', rea') = OpacityElim.opacity_elimination(rea_im, topdec)

		      val _ = Name.bucket := []
		      val (intB', modc) = IntModules.interp(intB_im, topdec', filename)
		      val names_int = !Name.bucket
		      val _ = Name.bucket := []

		      (* match export elaboration and interpretation
		       * bases to those found in repository. *)

		      val _ = match_elab(names_elab, elabB', funid)
		      val _ = match_int(names_int, intB', funid)

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
		  end
		) handle XX => (log_cleanup(); raise XX)
	  end  

    (* ----------------
     * build a unit
     * ---------------- *)

    fun build_unit(B, (funid, filepath): punit) : Basis * modcode =
      let val funstamp_now = FunStamp.from_filemodtime filepath  (*always get funstamp before reading content*)
	  exception CAN'T_REUSE
      in (case (Repository.lookup_elab funid, Repository.lookup_int funid)
	    of (Some(_,(infB, elabB, (rea,dom_rea), names_elab,infB',elabB', rea')), 
		Some(_,(funstamp,elabE,intB,names_int,modc,intB'))) =>
	      let val B_im = Basis.mk(infB,elabB,rea,intB)
	      in if FunStamp.eq(funstamp,funstamp_now) andalso
		     let (* val _ = print "\n[checking enrichment ...\n" *)
		         val res = Basis.enrich(B, (B_im, dom_rea))
			 (* val _ = print " done]\n" *)
		     in res
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
		    in List.apply Name.unmark_gen names_elab;    (* unmark names - they where *)
		       List.apply Name.unmark_gen names_int;     (* marked in the repository. *)
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
		    
    fun build_proj(B: Basis, project: project) : modcode =
      case project
	of [] => ModCode.empty
	 | (punit::project') => 
	  let val (B', modc) = build_unit(B, punit)
	      (* val _ = print "\n[adding result to basis ...\n" *)
	      val B'' = Basis.plus(B,B')
	      (* val _ = print " done]\n" *)
	      val modc' = build_proj(B'', project')
	  in ModCode.seq(modc,modc')
	  end

    (* ------------------------------------
     * build()  builds a loaded project
     * ------------------------------------ *)
			
    fun build() =
      let val _ = Repository.recover()
	  val emitted_files = EqSet.fromList (Repository.emitted_files())
	  val _ = let val modc = build_proj(Basis.initial, !project)
		  in ModCode.mk_exe (modc, "run")
		  end handle PARSE_ELAB_ERROR => output(std_out, "\n ** Parse or elaboration error occurred. **\n")
	  val emitted_files' = EqSet.fromList (Repository.emitted_files())
    	  val files_to_delete = EqSet.list (EqSet.difference emitted_files emitted_files')
      in List.apply ManagerObjects.SystemTools.delete_file files_to_delete
      end

    (* -----------------------------
     * Compile a single file 
     * ----------------------------- *)

    fun comp (filename : string) : unit =
      let val (infB,elabB,rea,intB) = Basis.un Basis.initial
	  val _ = reset()
	  val _ = Timing.reset_timings()
	  val _ = Timing.new_file filename
	  val log_cleanup = log_init filename
      in (case ParseElab.parse_elab {infB=infB,elabB=elabB,
				     file=filename_to_sourcefile filename} 
	    of ParseElab.SUCCESS {report, topdec, ...} =>
	      let val (topdec',_) = OpacityElim.opacity_elimination(rea,topdec)
		  val (_, modc) = IntModules.interp(intB, topdec', filename)
		  val modc = ModCode.emit modc
	      in ModCode.mk_exe(modc, filename ^ ".exe");
	         print_result_report report;
		 log_cleanup()
	      end
	     | ParseElab.FAILURE report => (print_error_report report; raise PARSE_ELAB_ERROR)
         ) handle XX => (log_cleanup(); raise XX)
      end 


    (* -----------------------------
     * Elaborate a single file 
     * ----------------------------- *)

    fun elab (filename : string) : unit =
      let val (infB,elabB,_,_) = Basis.un Basis.initial
	  val _ = reset()
	  val log_cleanup = log_init filename
      in (case ParseElab.parse_elab {infB=infB,elabB=elabB,
				     file=filename_to_sourcefile filename} 
	    of ParseElab.SUCCESS {report, ...} => (print_result_report report; log_cleanup())
	     | ParseElab.FAILURE report => (print_error_report report; log_cleanup())
	 ) handle E => (log_cleanup(); raise E)
      end 


    (* initialize Flags.build_ref to contain build (for interaction), etc.
     * See comment in FLAGS.*)

    val _ = Flags.build_project_ref := build
    val _ = Flags.show_project_ref := show
    val _ = Flags.read_project_ref := read
    val _ = 
      let val comp = fn a => 
	((comp a) handle PARSE_ELAB_ERROR => output(std_out, "\n ** Parse or elaboration error occurred. **\n"))
      in Flags.comp_ref := comp
      end

    val interact = Flags.interact
    val read_script = Flags.read_script

  end
