(*$Manager: BASIS NAME PARSE_ELAB CRASH REPORT PRETTYPRINT FLAGS
            COMPILE FREE_IDS MANAGER TIMING*)

functor Manager(structure Basis : BASIS
		structure Name : NAME
		  sharing type Name.name = Basis.name
		structure ParseElab : PARSE_ELAB
		  sharing type ParseElab.Basis = Basis.Basis
		structure Compile : COMPILE 
		  sharing type Compile.CompileBasis = Basis.CompileBasis
		      and type Compile.topdec = ParseElab.topdec
		structure FreeIds : FREE_IDS
		  sharing type FreeIds.ids = Basis.ids
		      and type FreeIds.topdec = ParseElab.topdec
	        structure Timing : TIMING
		structure Crash : CRASH
		structure Report : REPORT
		  sharing type Report.Report = ParseElab.Report
		structure PP : PRETTYPRINT
		  sharing type PP.StringTree = FreeIds.StringTree = Basis.StringTree
                structure Flags : FLAGS) : MANAGER =
  struct
    fun die s = Crash.impossible ("Manager." ^ s)
    fun log (s:string) : unit = output (!Flags.log, s)
    fun log' s = log (s ^ "\n")
    fun log_st (st) : unit = PP.outputTree (log, st, 70)


   (* -----------------------------------------------------------------
    * Dynamic flags
    * ----------------------------------------------------------------- *)

    val debug_expbases = ref false
    val report_file_sig = ref false
    val _ = Flags.add_flag_to_menu
          (["Debug Kit","Manager"], "debug_expbases",
	   "debug expbases", debug_expbases)
    val _ = Flags.add_flag_to_menu
          (["Control"], "report_file_sig",
	   "report types, values, etc.", report_file_sig)
    val region_profiling = Flags.lookup_flag_entry "region_profiling"
    val c_compiler = Flags.lookup_string_entry "c_compiler"
    val c_libs = Flags.lookup_string_entry "c_libs"
    val log_to_file = Flags.lookup_flag_entry "log_to_file"


   (* -----------------------------------------------------------------
    * Execute shell command and return the result code.
    * ----------------------------------------------------------------- *)

    structure Shell =
      struct
	exception Execute of string

	fun execute_shell_command command : unit =
	  let val error_code = SML_NJ.system command
	  in if error_code <> 0 then
	        raise Execute ("Error code " ^ Int.string error_code ^
			       " when executing shell command:\n"
			       ^ command)
	     else ()
	  end
      end

    fun assemble file file' =
          Shell.execute_shell_command (!c_compiler ^ " -c -o "
				       ^ file' ^ " " ^ file)
	  (*e.g., "cc -Aa -c -o link.o link.s"

	   man cc:
	   -c          Suppress the link edit phase of the compilation, and
		       force an object (.o) file to be produced for each .c
		       file even if only one program is compiled.  Object
		       files produced from C programs must be linked before
		       being executed.

	   -ooutfile   Name the output file from the linker outfile.  The
		       default name is a.out.*)
	  handle Shell.Execute s => die ("assemble: " ^ s)

    (* -----------------------------------------
     * Directories and linked runtime system
     * ----------------------------------------- *)

    val source_directory = Flags.lookup_string_entry "source_directory"
    val target_directory = Flags.lookup_string_entry "target_directory"
    val log_directory = Flags.lookup_string_entry "log_directory"
    val link_filename = Flags.lookup_string_entry "link_filename"
    val target_file_extension = Flags.lookup_string_entry "target_file_extension"

    fun path_to_source_file file = !source_directory ^ file ^ ".sml"
    fun path_to_target_file file = !target_directory ^ file ^ !target_file_extension
    fun path_to_log_file    file = !log_directory    ^ file ^ ".log"
    fun path_to_vcg_file    file = !target_directory ^ file ^ ".vcg"
    fun path_to_runtime () = ! (Flags.lookup_string_entry
				(if !region_profiling then "path_to_runtime_prof"
				 else "path_to_runtime"))

    (* -----------------------------------------
     * Choosing a fresh file name 
     * ----------------------------------------- *)

    local val count = ref 0
          fun new () = (count := !count + 1; !count)
    in fun new_target_filename file = !target_directory ^ file (*^ Int.string (new ())*)
       fun append_ext s = s ^ !target_file_extension (*".s"21/03/1997, Niels*)
       fun append_o s = s ^ ".o"
    end

    type Basis = Basis.Basis
    type ExpBasis = Basis.ExpBasis

    exception Error and Error1
    fun error (s : string) = (print "**ERROR**\n";
			      print s; print "\n";
			      die "")

    type target_filename = string
    type linkinfo = Compile.linkinfo

    type filename = string

    datatype unit_state = NEW of filename
                        | OK  of Basis * filename * ExpBasis
                                 * (target_filename * linkinfo) Option
                        | TOUCHED of filename * ExpBasis
    fun file_name_of_unit_state (NEW s) = s
      | file_name_of_unit_state (OK(_,s,_,_)) = s
      | file_name_of_unit_state (TOUCHED (s,_)) = s
 
    type program_state = unit_state list

    val program_state = ref ([] : program_state)

    (*show ()  show the program state.*)
    fun show () : unit =
      let fun max(a:int,b) = if a > b then a else b
	  fun sz [] m = m
	    | sz (s::rest) m = sz rest (max(size s,m))
	  val sz = 2 + sz (map file_name_of_unit_state (!program_state)) 0
	  fun lay (NEW s) = String.padR " " sz s ^ "new"
	    | lay (OK(_,s,_,None)) = String.padR " " sz s ^ "ok(empty)"
	    | lay (OK(_,s,_,Some(t,_))) = String.padR " " sz s ^ "ok(" ^ t ^ ")"
	    | lay (TOUCHED(s,_)) = String.padR " " sz s ^ "touched"
	  fun pr [] = ()
	    | pr (x::xs) = (print " "; print x; print "\n"; pr xs)
      in print (String.padR "-" sz "" ^ "---------\n"); 
	 pr (map lay (!program_state));
	 print (String.padR "-" sz "" ^ "---------\n")
      end

    (* load(name)  loads project file `name' into program state. *)
    fun load (s : string) : unit =  
      let val s = !source_directory ^ s
          val file_string = StringParse.fromFile s
	  val files = StringParse.words {groups=" \n\t", singles="",
					 preserveGroups=false, preserveSingles=false} file_string
	  fun elim []= []
	    | elim (""::fs) = elim fs
	    | elim (f ::fs) = f :: elim fs
	  val files = elim files 
      in
	Compile.reset ();
	program_state := map NEW files
      end handle Io io_s => (error ("Project file \"" ^ s ^ "\" cannot be opened."));

	
    (* save(name)  saves a project file `name' from program state. *)
    fun save(name : string) : unit =
      case !program_state
	of [] => error "Program state is empty."
	 | _ => (let val name = !source_directory ^ name
		     val file_names = map file_name_of_unit_state (!program_state)
		     val file_names = case rev file_names 
					of [] => []
					 | (x::xs) => rev (x :: map (fn s => s ^ "\n") xs)  
		     val s = implode file_names
		 in StringParse.file name s
		 end handle Io _ => error ("Failed to write to file \"" ^ name ^ "\"."))


    fun wipe () : unit = 
      let fun f (u as NEW _) = u
	    | f (OK (_,f,_,_)) = NEW f
	    | f (TOUCHED(f,_)) = NEW f
      in program_state := map f (!program_state)
      end

    fun touch (s : string) : unit =
      let fun f [] = raise Error
	    | f (ps as u :: rest) =
	       case u
		 of NEW t => if s = t then ps else u :: f rest
		  | OK(_,t,Bexp,_) => if s = t then TOUCHED(t,Bexp) :: rest else u :: f rest
		  | TOUCHED(t,_) => if s = t then ps else u :: f rest
      in program_state := f (!program_state)  
      end handle Error => error ("Program unit \"" ^ s ^ "\" not in program state.")


    fun touch_all () : unit =
          (List.apply touch o map file_name_of_unit_state o !) program_state

    (* delete(name)  deletes `name' from program state. *)
    fun delete (s : string) : unit =
      let fun f [] = raise Error
	    | f (ps as u :: rest) =
	      let fun g t = if s = t then rest else u :: f rest
	      in case u
		   of NEW t => g t
		    | OK(_,t,_,_) => g t
		    | TOUCHED(t,_) => g t
	      end
      in program_state := f (!program_state)  
      end handle Error => error ("Program unit \"" ^ s ^ "\" not in program state.")
      

    (* add(name)  adds `name' to the end of program state *)
    fun add (s : string) : unit =
      let fun f [] = [NEW s]
	    | f (ps as u :: rest) =
	      let fun g t = if s = t then raise Error else u :: f rest
	      in case u
		   of NEW t => g t 
		    | OK(_,t,_,_) => g t
		    | TOUCHED(t,_) => g t
	      end
      in program_state := f (!program_state)  
      end handle Error => error ("Program unit \"" ^ s ^ "\" already in program state.") 
      

    (* insert(name, name0)  inserts `name' before `name0' in program state. *)
    fun insert(name:string, name0:string) : unit =
      let fun f ([], false) =  (*not seen yet*) raise Error1
	    | f ([], true) = []
	    | f (ps as u :: rest, b) =
	      let fun g t = if name = t then raise Error 
			    else if name0 = t then NEW name :: u :: f (rest, true) 
				 else u :: f (rest, b)
	      in case u
		   of NEW t => g t 
		    | OK (_,t,_,_) => g t
		    | TOUCHED (t,_) => g t
	      end 
      in program_state := f(!program_state, false)
      end handle Error => error ("Program unit \"" ^ name ^ "\" already in program state.")
               | Error1 =>  error ("Program unit \"" ^ name0 ^ "\" not in program state.")

	
    (* ---------------------------------------
     * Reset and commit
     * --------------------------------------- *)
	
    fun reset() = Compile.reset()
    fun commit() = Compile.commit()
	
    (* ------------------------------------------- 
     * Debugging and reporting
     * ------------------------------------------- *)

    fun debug_free_ids ids =
      (log ("\nFree ids:");
       log_st (FreeIds.layout_ids ids);
       log "\n")

    fun debug_basis s B =
      (log ("\n" ^ s ^ " static basis:");
       log_st (Basis.layout_StaticBasis (Basis.Stat_of_B B));
       log ("\n" ^ s ^ " compiler basis:");
       log_st (Basis.layout_CompileBasis (Basis.Comp_of_B B));
       log "\n\n")

    fun pr_name n = "n" ^ Int.string (Name.key n)
    fun pr_names ns = "[" ^ pr_names' ns ^ "]"
    and pr_names' [] = ""
      | pr_names' [n] = pr_name n
      | pr_names' (n::ns) = pr_name n ^ "," ^ pr_names' ns
 
    fun debug_expbasis s F =
      let val (N, B) = Basis.de_ExpBasis F
      in log("EXPBASIS:: names = " ^ pr_names N ^ "\n");
	 debug_basis s B
      end

    fun print_report report = Report.print' report (!Flags.log)

    fun reset_warnings() = Flags.warnings := []
    fun report_warnings() = let val warnings = rev (!Flags.warnings)
				val _ = reset_warnings()
			    in case warnings
				 of [] => ()
				  | _ => (output(std_out, "***Printed warnings on log file ***\n");
                                          log "\n\n *** Warnings ***\n";
					  List.apply log' warnings;
					  log "\n\n")
			    end 



    (*parse_elab_compile (B, "/usr/a/p.sml", "/usr/a/out/p.log",
                             "/usr/a/out/p.vcg") =
     parse, elaborate, and compile "/usr/a/p.sml" in basis `B'.
     Produce log information on "/usr/a/out/p.log" if !log_to_file,
     otherwise on std_out.  Produce a region flow graph on
     "/usr/a/out/p.vcg" if !Flags.gen_vcg_graph.
     Report on log and raise exception PARSE_ELAB_ERROR if parsing or
     elaboration fails.*)

    exception PARSE_ELAB_ERROR
    fun parse_elab_compile (B, source_file, log_file, vcg_file) =
          let 
	    val _ = Timing.new_file(source_file)
	    val old_log_stream = !Flags.log
	    val log_stream =
	          if !log_to_file then
		    open_out log_file
		    handle Io msg => die ("Cannot open log file\n\
		                          \(non-exsisting directory or write-\
					  \protected existing log file?)\n"
		                          ^ msg)
		  else std_out

	    fun cleanup () : unit =
	          (Flags.log := old_log_stream ;
		   if !log_to_file then (*close log file*)
		     (output (std_out, "[wrote log file:\t" ^ log_file ^ "]\n") ;
		      close_out log_stream)
		   else ())
	  in
	    Flags.log := log_stream ;
	    if !log_to_file then
	      output (log_stream, "\n\n********** "
		      ^ source_file ^ " *************\n\n")
	    else () ;
	    Name.bucket := [] ;
	    reset_warnings () ;
	    let
	      val (report,B',topdec) =
		    (case ParseElab.parse_elab {Basis=B, file=source_file} of
		       ParseElab.FAILURE report =>
			 (print_report report;
			  if !log_to_file then 
			    output (std_out, "\n***\n***Errors during parsing or \
			            \elaboration. See log file.\n***\n\n") else () ;
			  raise PARSE_ELAB_ERROR)
		     | ParseElab.SUCCESS {report,basis,topdec} =>
			 (report,basis,topdec))

	      val ids = FreeIds.free_ids topdec
	      (* val _ = debug_free_ids ids *)
	      val Bimp = Basis.restrict(B,ids)
	      (* val _ = debug_basis "Import" Bimp *)

	      val (B', opt) =
		    (case Compile.compile (Basis.Comp_of_B Bimp,
					   topdec, vcg_file) of
		       Some (CB', target, linkinfo) => 
			 let val B' = Basis.B_plus_B(B', Basis.Comp_in_B CB')
			 in (B', Some (target, linkinfo))
			 end
		     | None => (B', None))
	      val F = Basis.mk_ExpBasis(!Name.bucket, B')
	    in
	      Name.bucket := [] ;
	      if !report_file_sig then print_report report else () ;
	      report_warnings () ;         (*always report warnings*)
	      cleanup ();
	      (Bimp, F, opt)
	    end handle X => (cleanup (); raise X)
	  end
  

    (* ------------------------------------------------------------
     * Optionally, emit assembler code and partially link it. 
     * ------------------------------------------------------------ *)

    fun opt_emit None _ = None
      | opt_emit (Some (target, linkinfo)) file =
      let val target_filename = new_target_filename file
	  val target_filename_s = append_ext target_filename
	  val target_filename_o = append_o target_filename
	  val _ = Compile.emit {target=target,filename=target_filename_s}
	  val _ = assemble target_filename_s target_filename_o
      in Some (target_filename_o, linkinfo)
      end


    (* -------------------------------------------------------------
     * link_files_with_runtime_system files run : Link a list `files' of
     * partially linked files (.o files) to the runtime system
     * (also partially linked) and produce an executable called `run'.
     * ------------------------------------------------------------- *)

    fun link_files_with_runtime_system files run =
          let val files = map (fn s => s ^ " ") files
	  in
	    (Shell.execute_shell_command
	     (!c_compiler ^ " -o " ^ run ^ " " ^ implode files
	      ^ path_to_runtime () ^ " " ^ !c_libs)
              (*see comment at `assemble' above*);
	     output (std_out, "[wrote executable file:\t" ^ run ^ "]\n"))
	  end handle Shell.Execute s => die ("link_files_with_runtime_system:\n" ^ s)


    (* --------------------------------------------------------------
     * link (): Extract linkinfo from each program unit and produce a
     * link file "link.s". Then link the entire system and produce an
     * executable "run".
     * -------------------------------------------------------------- *)

    fun link () : unit =
      let val (target_files, linkinfos) = 
	    List.foldL (fn OK(_,_,_,None) => (fn acc => acc)
	                 | OK(_,_,_,Some(f,li)) => (fn (acc_files,acc_infos) =>
						    (f::acc_files,li::acc_infos))
			 | _ => die "Program not linkable.")
	    ([],[]) (!program_state)
	  val target_link = Compile.generate_link_code (rev linkinfos)
	  val linkfile = !target_directory ^ !link_filename 
	  val linkfile_s = append_ext linkfile
	  val linkfile_o = append_o linkfile
	  val _ = Compile.emit {target=target_link, filename=linkfile_s}
	  val _ = assemble linkfile_s linkfile_o
      in link_files_with_runtime_system (linkfile_o :: target_files) (!target_directory ^ "run")
      end


    (* ---------------------------------------------------------------
     * build(): Build the entire system, using information about
     * previously compiled program units.
     * --------------------------------------------------------------- *)

    fun parse_elab_compile' (B, file) = (*may raise PARSE_ELAB_ERROR*)
          parse_elab_compile (B, path_to_source_file file,
			      path_to_log_file file, path_to_vcg_file file)

    fun build () : unit =
      let 
	infix ++ 
	val op ++ = Basis.B_plus_Bexp
	val _ = Timing.reset_timings()
	fun loop (B, [], acc) = (program_state := rev acc; link ())
	  | loop (B, u::us, acc) =
	  (case u of
	     NEW file =>
	       let val (Bimp, F, opt_target) = parse_elab_compile' (B, file)
		 val _ = if !debug_expbases
			   then debug_expbasis (" " ^ file ^ " NEW:") F else ()
		 val opt_target' = opt_emit opt_target file
	       in loop (B ++ F, us, OK (Bimp, file, F, opt_target') :: acc)
	       end
	   | TOUCHED (file,F0) =>
	       let val (Bimp, F, opt_target) = parse_elab_compile' (B,file)
		 val F = Basis.match (F,F0)
		 val _ = if !debug_expbases
			   then debug_expbasis (" " ^ file ^ " TOUCHED:") F else ()
		 val opt_target' = opt_emit opt_target file
	       in loop (B ++ F, us, OK (Bimp, file, F, opt_target') :: acc)
	       end
	   | OK (Bimp, file, F, opt_target) =>
	       if Basis.enrich (B,Bimp) then loop (B ++ F, us, u :: acc)
	       else loop (B, TOUCHED (file, F) :: us, acc))
	     handle PARSE_ELAB_ERROR => (program_state := rev acc @ (u::us);
					 raise PARSE_ELAB_ERROR)
      in loop (Basis.initialB, !program_state, [])
      end

    (* -------------------------------------------------------
     * Compilation of a single program unit; used by TestEnv,
     * and by comp below.
     * ------------------------------------------------------- *)

    fun compile {sourcename,targetname,linkname,logname,vcgname} = 
         let  
	   val _ = Timing.reset_timings()
	   val (Bimp, F, opt_target) = 
	     parse_elab_compile(Basis.initialB, sourcename, logname, vcgname)
         in case opt_target
	      of Some(target,linkinfo) =>
		let val link = Compile.generate_link_code [linkinfo]
		in Compile.emit {target=target,filename=targetname};
		  Compile.emit {target=link, filename=linkname};
		  true
		end
	    | None => false
	 end (*handle PARSE_ELAB_ERROR => false23/04/1997, Niels*)
(*KILL 28/03/1997 22:27. tho.:
 handle PARSE_ELAB_ERROR => 
                      (output (std_out, "Errors during parsing or elaboration. See log file.\n");
                       false)
*)

    (* -------------------------------------------------------
     * Easy compilation of a single program unit.
     * ------------------------------------------------------- *)

    fun comp file =
          (compile {sourcename = path_to_source_file file,
		    targetname = path_to_target_file file,
		    linkname   = !target_directory ^ !link_filename ^ !target_file_extension,
		    logname    = path_to_log_file file,
		    vcgname    = path_to_vcg_file file}; ())


    (* initialize Flags.build_ref to contain build (for interaction), etc.
     See comment in FLAGS.*)

    val _ = Flags.build_ref := build
    val _ = Flags.show_ref := show
    val _ = Flags.load_ref := load
    val _ = Flags.touch_ref := touch
    val _ = Flags.touch_all_ref := touch_all
    val _ = Flags.comp_ref := comp ;

    val interact = Flags.interact
    val read_script = Flags.read_script

    fun issue_warnings () = 
          (case !Flags.warnings of
	     [] =>  ()
	   | [s] => (output(std_out, "\n*** 1  warning printed on the log file\n");
		     output(!Flags.log, "*** warning: " ^ s))
	   | ss =>  (output(std_out, "\n*** " ^ Int.string (length ss)
			    ^ " warnings printed on the log file\n");
	             List.apply (fn s => output(!Flags.log, "*** warning: " ^ s)) (rev ss)))

    fun elab (file : string) : unit =
          (Flags.warnings := [] ;
	   print_report
	     (case ParseElab.parse_elab {Basis=Basis.initialB,
					 file = path_to_source_file file} of
		ParseElab.SUCCESS {report, basis, topdec} => report 
	      | ParseElab.FAILURE report => report) ;
	   issue_warnings ())

  end
