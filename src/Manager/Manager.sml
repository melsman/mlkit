
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
		      and type FreeIds.longid = ManagerObjects.longid = ModuleEnvironments.longid = Environments.longid
		      and type FreeIds.longtycon = ManagerObjects.longtycon = ModuleEnvironments.longtycon = Environments.longtycon
		      and type FreeIds.longstrid = ManagerObjects.longstrid = ModuleEnvironments.longstrid = Environments.longstrid
		      and type FreeIds.funid = ManagerObjects.funid = ModuleEnvironments.funid
		      and type FreeIds.sigid = ManagerObjects.sigid = ModuleEnvironments.sigid
		structure OpacityElim : OPACITY_ELIM
		  sharing OpacityElim.TyName = Environments.TyName = ManagerObjects.TyName = ModuleEnvironments.TyName
		      and type OpacityElim.topdec = ParseElab.topdec
		      and type OpacityElim.opaq_env = ManagerObjects.opaq_env
		      and type OpacityElim.OpacityEnv.funid = FreeIds.funid
	        structure Timing : TIMING
		structure Crash : CRASH
		structure Report : REPORT
		  sharing type Report.Report = ParseElab.Report
		structure PP : PRETTYPRINT
		  sharing type PP.StringTree = FreeIds.StringTree = ManagerObjects.StringTree = OpacityElim.OpacityEnv.StringTree
                structure Flags : FLAGS) : MANAGER =
  struct

    structure StringParse = Edlib.StringParse
    structure EqSet = Edlib.EqSet

    structure Basis = ManagerObjects.Basis
    structure FunStamp = ManagerObjects.FunStamp
    structure ModCode = ManagerObjects.ModCode
    structure Repository = ManagerObjects.Repository
    structure IntBasis = ManagerObjects.IntBasis
    structure ElabBasis = ModuleEnvironments.B
    structure ErrorCode = ParseElab.ErrorCode

    type TopBasis = ManagerObjects.TopBasis


    fun die s = Crash.impossible ("Manager." ^ s)

    val region_profiling = Flags.lookup_flag_entry "region_profiling"

    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list
    fun error (s : string) = (print ("\nError: " ^ s ^ ".\n\n"); raise PARSE_ELAB_ERROR[])
    fun quot s = "`" ^ s ^ "'"

    fun member s l = let fun m [] = false
			   | m (x::xs) = x = s orelse m xs
		     in m l
		     end

    (* -----------------------------------------
     * Unit names, file names and directories
     * ----------------------------------------- *)

    type filename = ManagerObjects.filename       (* At some point we should use *)
                                                  (* abstract types for these things *)
     and funid = ManagerObjects.funid             (* so that we correctly distinguish *)
     and funstamp = ManagerObjects.funstamp       (* unit names and file names. *)

    fun unitname_to_logfile unitname = unitname ^ ".log"
    fun unitname_to_sourcefile unitname = unitname (*mads ^ ".sml"*)
    fun filename_to_unitname (f:string) : string = f

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

    fun drop_comments prjid (l: char list) : char list =
      let fun loop(n, #"(" :: #"*" :: rest ) = loop(n+1, rest)
	    | loop(n, #"*" :: #")" :: rest ) = loop(n-1, if n=1 then #" "::rest else rest)
	    | loop(0, ch ::rest) = ch :: loop (0,rest)
	    | loop(0, []) = []
	    | loop(n, ch ::rest) = loop(n,rest)
	    | loop(n, []) = error ("Unclosed comment in project " ^ quot prjid)
      in loop(0,l)
      end
	
    (* ------------------------------------------- 
     * Debugging and reporting
     * ------------------------------------------- *)

    fun debug_free_longids longids =
      (log ("\nFree longids:");
       log_st (FreeIds.layout_longids longids);
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

    type extobj = string   (* externally compiled objects; .o-files *)
    type prjid = string
    type unitid = string
    datatype body = SEQbody of body list
                  | LOCALbody of body * body
                  | UNITbody of unitid 
    type prj = {imports : prjid list, extobjs: extobj list, body : body}


    fun parse_project (prjid : prjid) : prj =
      let
 
        fun parse_error s' = error ("while parsing project: " ^ quot prjid ^ " : " ^ s')
        fun parse_error1(s', rest: string list) = 
          case rest of 
            [] => error ("while parsing project: " ^ quot prjid ^ " : " ^ s' ^ "(reached end of file)")
          | s::_ => error ("while parsing project: " ^ quot prjid ^ " : " ^ s' ^ "(reached `" ^ s ^ "')")
             

	fun has_ext(s,ext) = case OS.Path.ext s
			       of SOME ext' => ext = ext'
				| NONE => false

	val _ = if has_ext(prjid, "pm") then ()
		else error ("Your project file " ^ quot prjid ^ " does not have extension `pm'")

	fun is_whitesp #"\n" = true
	  | is_whitesp #" " = true
	  | is_whitesp #"\t" = true
	  | is_whitesp _ = false

	fun is_colon #":" = true
	  | is_colon _ = false

	fun lex_colon (#":"::chs) = SOME chs
	  | lex_colon _ = NONE

	fun lex_whitesp (all as c::rest) = if is_whitesp c then lex_whitesp rest
					   else all 
	  | lex_whitesp [] = []

	fun lex_string (c::rest, acc) = if is_colon c then (implode(rev acc), c::rest)
					else if is_whitesp c then (implode(rev acc), rest)
					     else lex_string (rest, c::acc)
	  | lex_string ([], acc) = (implode(rev acc), [])

	fun lex (chs : char list, acc) : string list =
	  case lex_whitesp chs
	    of [] => rev acc
	     | chs => lex (case lex_colon chs
			     of SOME chs => (chs, ":"::acc)
			      | NONE => let val (s, chs) = lex_string(chs,[])
					in (chs, s::acc) 
					end)
	val lex = fn chs => lex(chs,[])

	fun parse_body_opt (ss : string list) : (body * string list) option =
	  case ss
	    of [] => NONE
	     | "local" :: ss =>
	      let fun parse_rest'(body1,body2,ss) =
		    case ss
		      of "end" :: ss => 
			(case parse_body_opt ss
			   of SOME(body',ss) => SOME(SEQbody[LOCALbody(body1,body2),body'], ss)
			    | NONE => SOME(LOCALbody(body1,body2), ss))
		       | _ => parse_error1 ("I expect an `end'.) ", ss)

		  fun parse_rest(body1,ss) =
		    case ss
		      of "in" :: ss => 
			(case parse_body_opt ss
			   of SOME(body2,ss) => parse_rest'(body1,body2,ss)
			    | NONE => parse_rest'(body1,SEQbody[],ss))
		       | _ => parse_error1( "I expect an `in'", ss)
	      in case parse_body_opt ss
		   of SOME(body1,ss) => parse_rest(body1,ss)
		    | NONE => parse_rest(SEQbody[],ss)
	      end
	     | s :: ss => 
	      if has_ext(s,"sml") orelse has_ext(s,"sig") then 
		case parse_body_opt ss
		  of SOME (body', ss) => SOME(SEQbody[UNITbody s, body'], ss)
		   | NONE => SOME(UNITbody s, ss)
	      else NONE
		
        fun parse_prj (ss : string list) : prj =
	  let fun parse_end({prjids,objs}, body, ss) =
	        case ss
		  of [] => {imports=prjids,extobjs=objs,body=body}
		   | _ => parse_error1( "I expect end of file", ss)
	  in case ss
	       of [] => {imports=[],extobjs=[],body=SEQbody[]}
		| "import" :: ss =>
		 let fun parse_rest'(prjids_objs,body,ss) =
		       case ss
			 of "end" :: ss => parse_end(prjids_objs,body,ss)
			  | _ => parse_error1( "I expect an `end'", ss)
		     fun parse_rest(prjids_objs, ss) =
		       case ss
			 of "in" :: ss =>
			   (case parse_body_opt ss
			      of SOME(body, ss) => parse_rest'(prjids_objs,body,ss)
			       | NONE => parse_rest'(prjids_objs,SEQbody[],ss))
			  | _ => parse_error1( "I expect an `in'", ss)
		 in case parse_prjids_opt ss
		      of SOME(prjids_objs,ss) => parse_rest(prjids_objs,ss)
		       | NONE => parse_rest({prjids=[],objs=[]},ss)
		 end
		| _ => (case parse_body_opt ss
			  of SOME(body,ss) => parse_end({prjids=[],objs=[]},body,ss)
			   | NONE => parse_error( "I expect an `import' or a body"))
	  end

	and parse_prjids_opt ss : ({prjids:string list, objs:string list} * string list) option =
	  case ss
	    of s :: ss =>
	      if has_ext(s,"pm") then 
		case parse_prjids_opt ss
		  of SOME({prjids,objs},ss) => SOME({prjids=s::prjids,objs=objs}, ss)
		   | NONE => SOME({prjids=[s],objs=[]}, ss)
	      else if has_ext(s, "o") then
		let fun parse_with_obj(obj, ss) =
		      case parse_prjids_opt ss
			of SOME({prjids,objs},ss) => SOME({prjids=prjids,objs=obj::objs}, ss)
			 | NONE => SOME({prjids=[],objs=[obj]}, ss)
		in case ss
		     of ":"::s'::ss' =>
		       if has_ext(s', "o") then
			 let val s'' = if !region_profiling then s' else s
			 in parse_with_obj(s'', ss')
			 end
		       else parse_error("I expected " ^ s' ^ " (occuring after a `:') to have extension `.o'.")
		      | _ => 
			 if !region_profiling then 
			   parse_error("I expected a `:' and an external object (.o file) to use\n" ^
				       "now when profiling is enabled.")
			 else parse_with_obj(s, ss)
		end 
	      else NONE
	     | _  => NONE

	val prj = (parse_prj o lex o (drop_comments prjid) o explode o StringParse.fromFile) prjid
	  handle IO.Io {name=io_s,...} => error ("The project " ^ quot prjid ^ " cannot be opened")

      in prj
      end

    fun local_check_project (prjid0, {imports,extobjs,body}) : unit =
      let fun check_imports (_,[]) = ()
	    | check_imports (P, prjid :: rest) =
	     let val prjid = OS.Path.file prjid
	     in if member prjid P then error ("The project " ^ quot prjid ^ 
					      " is imported twice in project " ^ quot prjid0)
		else check_imports(prjid::P,rest)
	     end
	  fun check_extobj extobj =
	     let val extobj = OS.Path.file extobj
	         fun exists file = OS.FileSys.access(file,[])
	     in if not(exists extobj) then error ("The external object file " ^ quot extobj ^ 
						  " imported in project " ^ quot prjid0 ^ 
						  " does not exist; first, compile this file.") 
		else ()
	     end
	  fun check_body (U, body) =
	    case body
	      of SEQbody[] => U
	       | SEQbody(body::rest) => check_body(check_body(U,body),SEQbody rest)
	       | LOCALbody(body1,body2) => check_body(check_body(U,body1), body2)
	       | UNITbody longunitid => 
		let val unitid = OS.Path.file longunitid
		in if member unitid U then 
                      error ("The program unit " ^ quot unitid ^ " is included twice in project " ^ quot prjid0)
		   else unitid::U
		end
      in check_body([], body); check_imports([], imports); List.app check_extobj extobjs
      end


    type Basis = ManagerObjects.Basis
    type modcode = ManagerObjects.modcode

    (* Matching of export elaboration and interpretation bases to
     * those in repository for a given funid *)

    fun match_elab(names_elab, elabB, opaq_env, prjid, funid) =
      case Repository.lookup_elab (prjid,funid)
	of SOME (_,(_,_,_,_,names_elab',_,elabB',opaq_env')) => (* names_elab' are already marked generative - lookup *)
	  (List.app Name.mark_gen names_elab;                   (* returned the entry. The invariant is that every *)
	   ElabBasis.match(elabB, elabB');                      (* name in the bucket is generative. *)
	   OpacityElim.OpacityEnv.match(opaq_env,opaq_env');
	   List.app Name.unmark_gen names_elab)
	 | NONE => () (*bad luck*)

    fun match_int(names_int, intB, prjid, funid) =
      case Repository.lookup_int (prjid,funid)
	of SOME(_,(_,_,_,_,names_int',_,intB')) =>   (* names_int' are already marked generative - lookup *)
	  (List.app Name.mark_gen names_int;         (* returned the entry. The invariant is that every *)
	   IntBasis.match(intB, intB');              (* name in the bucket is generative. *)
	   List.app Name.unmark_gen names_int)
	 | NONE => () (*bad luck*)

    (* --------------------------------
     * Parse, elaborate and interpret
     * (may raise PARSE_ELAB_ERROR)
     * -------------------------------- *)

    fun fid_topdec a = FreeIds.fid_topdec a
    fun ElabBasis_restrict a = ElabBasis.restrict a
    fun IntBasis_restrict a = IntBasis.restrict a
    fun IntBasis_restrict' a = IntBasis.restrict' a
    fun OpacityElim_restrict a = OpacityElim.OpacityEnv.restrict a
    fun opacity_elimination a = OpacityElim.opacity_elimination a

    fun parse_elab_interp (prjid,B, funid, punit, funstamp_now) : Basis * modcode =
          let val _ = Timing.reset_timings()
	      val _ = Timing.new_file punit
	      val (infB, elabB, opaq_env, topIntB) = Basis.un' B
	      val unitname = (filename_to_unitname o ManagerObjects.funid_to_filename) funid
	      val log_cleanup = log_init unitname
	      val _ = Name.bucket := []
	      val _ = Flags.reset_warnings ()
	      val _ = print("[reading source file:\t" ^ punit ^ "]\n")
	      val res = ParseElab.parse_elab {prjid=prjid,infB=infB,elabB=elabB, file=punit} 
	  in (case res
		of ParseElab.FAILURE (report, error_codes) => (print_error_report report; raise PARSE_ELAB_ERROR error_codes)
		 | ParseElab.SUCCESS {report,infB=infB',elabB=elabB',topdec} =>
		  let val names_elab = !Name.bucket

		      val _ = chat "[finding free identifiers begin...]\n"
		      val freelongids as {longvids,longtycons,longstrids,funids,sigids} = fid_topdec topdec
		      val _ = chat "[finding free identifiers end...]\n"

		      (* val _ = debug_free_ids ids *)
		      val _ = chat "[restricting elaboration basis begin...]\n"
		      val elabB_im = ElabBasis_restrict(elabB,freelongids)
		      val _ = chat "[restricting elaboration basis end...]\n"
		      (* val _ = debug_basis "Import" Bimp *)

		      val _ = chat "[restricting interpretation basis begin...]\n"
		      val intB_im = IntBasis_restrict'(topIntB, {funids=funids,longstrids=longstrids,longtycons=longtycons,
								 longvids=longvids})
		      val _ = chat "[restricting interpretation basis end...]\n"

 		      val tynames_elabB_im = ElabBasis.tynames elabB_im
		      val opaq_env_im = OpacityElim_restrict(opaq_env,(funids,tynames_elabB_im))

		      val _ = chat "[opacity elimination begin...]\n"
		      val (topdec', opaq_env') = opacity_elimination(opaq_env_im, topdec)
		      val _ = chat "[opacity elimination end...]\n"

		      val _ = chat "[interpretation begin...]\n"
		      val _ = Name.bucket := []
		      val (intB', modc) = IntModules.interp(prjid, intB_im, topdec', unitname)
		      val names_int = !Name.bucket
		      val _ = Name.bucket := []
		      val _ = chat "[interpretation end...]\n"

		      (* match export elaboration and interpretation
		       * bases to those found in repository. *)

		      val _ = chat "[matching begin...]\n"
		      val _ = match_elab(names_elab, elabB', opaq_env', prjid, funid)
		      val _ = match_int(names_int, intB', prjid, funid)
		      val _ = chat "[matching end...]\n"

		      val _ = Repository.delete_entries (prjid,funid)

		      val _ = Repository.add_elab ((prjid,funid), (infB, elabB_im, longstrids, (opaq_env_im,tynames_elabB_im), 
								   names_elab, infB', elabB', opaq_env'))
		      val modc = ModCode.emit (prjid,modc)  (* When module code is inserted in repository,
							     * names become rigid, so we emit the module code. *)
		      val elabE' = ElabBasis.to_E elabB'
		      val _ = Repository.add_int ((prjid,funid), (funstamp_now, elabE', intB_im, longstrids, names_int, modc, intB'))
		      val B' = Basis.mk(infB',elabB',opaq_env',intB')
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
    fun Basis_agree a = Basis.agree a

    fun build_punit(prjid,B: TopBasis, punit : string, clean : bool) : Basis * modcode * bool * Time.time =
      (* The bool is a `clean' flag;  *)
      let
          val funid = ManagerObjects.funid_from_filename punit
          val (modtime, funstamp_now) = 
	    case (SOME(OS.FileSys.modTime punit)
		  handle _ => NONE, FunStamp.from_filemodtime punit)   (*always get funstamp before reading content*)
	      of (SOME modtime, SOME fs) => (modtime, fs)
	       | _ => error ("The program unit " ^ quot punit ^ " does not exist")
	  exception CAN'T_REUSE
      in (case (Repository_lookup_elab (prjid,funid), Repository_lookup_int (prjid,funid))
	    of (SOME(_,(infB, elabB, longstrids, (opaq_env,dom_opaq_env), names_elab, infB', elabB', opaq_env')), 
		SOME(_,(funstamp, elabE, intB, _, names_int, modc, intB'))) =>
	      if FunStamp.eq(funstamp,funstamp_now) andalso ModCode.exist modc then
		(if clean then (print ("[reusing code for: \t" ^ punit ^ "]\n");
				(Basis.mk(infB',elabB',opaq_env',intB'), modc, clean, modtime))
		 else if
		        let
			  val B_im = Basis.mk(infB,elabB,opaq_env,intB)
			  fun unmark_names () = (List.app Name.unmark_gen names_elab;    (* Unmark names - they where *)
						 List.app Name.unmark_gen names_int)     (* marked in the repository. *)
			  fun remark_names () = (List.app Name.mark_gen names_elab;      (*  If enrichment fails we remark *)
						 List.app Name.mark_gen names_int)       (* names; notice that enrichment of *)
		                                                                         (* elaboration bases requires all *)
			  val _ = unmark_names()                                         (* names be unmarked. Names in the *)
			  val res = Basis_enrich(B, (B_im, dom_opaq_env)) andalso        (* global basis are always unmarked. *)
			    Basis_agree(longstrids,B,(B_im, dom_opaq_env))
			in (if res then () else remark_names() ; res)
			end then 
	  		          (print ("[reusing code for: \t" ^ punit ^ " *]\n");
				   (Basis.mk(infB',elabB',opaq_env',intB'), modc, clean, modtime))

		 else raise CAN'T_REUSE)

	      else raise CAN'T_REUSE
	      
	     | _ => raise CAN'T_REUSE)

	handle CAN'T_REUSE =>
	  let val (Bex, modc) = parse_elab_interp (prjid,B, funid, punit, funstamp_now)
	  in (Bex, modc, false, modtime) (*not clean*)
	  end
      end 

    (* ----------------
     * build a project
     * ---------------- *)

    fun Basis_plus (B,B') = Basis.plus(B,B')		    
    fun Basis_plus' (B,B') = Basis.plus'(B,B')		    

    fun maybe_create_dir d : unit =
      if OS.FileSys.access (d, []) handle _ => error ("I cannot access directory " ^ quot d) then
	if OS.FileSys.isDir d then ()
	else error ("The file " ^ quot d ^ " is not a directory")
      else (OS.FileSys.mkDir d handle _ => error ("I cannot create directory " ^ quot d))

    fun maybe_create_PM_dir() : unit =
      (maybe_create_dir "PM"; maybe_create_dir "PM/Prof"; maybe_create_dir "PM/NoProf")
      
	 

    fun change_dir p : {cd_old : unit -> unit, file : string} =
      let val {dir,file} = OS.Path.splitDirFile p
      in if dir = "" then {cd_old = fn()=>(),file=file}
	 else let val old_dir = OS.FileSys.getDir()
	          val _ = OS.FileSys.chDir dir
	      in {cd_old=fn()=>OS.FileSys.chDir old_dir, file=file}
	      end handle OS.SysErr _ => error ("I cannot access directory " ^ quot dir)
      end

    fun build_body (prjid, B:TopBasis, Bacc, body, clean, modtimes) 
      : TopBasis * Basis * modcode * bool * (string * Time.time) list =  
      (* The bool is a `clean' flag; it is true if all entries 
       * of the project so far have been reused. *)
      (* The (string * Time.time) list provides modification times for
       * each of the source files mentioned in the project. *)
      case body
	of SEQbody [] => (B, Bacc, ModCode.empty, clean, modtimes)
	 | SEQbody (body :: bodys) => 
	  let val (B1, B1acc, modc1, clean, modtimes) = build_body (prjid, B, Bacc, body, clean, modtimes)
	      val (B2, B2acc, modc2, clean, modtimes) = build_body (prjid, B1, B1acc, SEQbody bodys, clean, modtimes)
	  in (B2, B2acc, ModCode.seq(modc1, modc2), clean, modtimes)
	  end
	 | LOCALbody _ => error "local not implemented"
	 | UNITbody unitid => 
	  let val {cd_old, file=unitid} = change_dir unitid
	  in let val _ = maybe_create_PM_dir()
	         val (B', modc', clean, modtime) = build_punit (prjid, B, unitid, clean)
	     in cd_old(); (Basis_plus'(B,B'), Basis_plus(Bacc,B'), modc', clean, (unitid,modtime)::modtimes)
	     end handle E => (cd_old(); raise E)
	  end
	

      (* Write a dummy file for the project into the `PM/Prof' or `PM/NoProf' directory. The date of this dummy
       * file tells when the project was last modified. *)

    fun output_date_file date_file =
      let val os = TextIO.openOut date_file
      in TextIO.output(os, "date"); TextIO.closeOut os
      end

    val older = Time.<


      (* We use two schemes for avoiding unnecessary recompilation. First, we use the modification time of a source
       * to avoid recompilation when nothing that comes earlier in a project has changed. A `clean' flag is used
       * to denote that nothing that comes earlier in a project has changed. Note that projects are closed, so
       * initially, when building a project, the clean flag is true. For each project file `file.pm' we associate
       * a dummy date file `file.pm.date' in the `PM/Prof' or `PM/NoProf' directory. The clean flag is preserved if
       *    (1) file.pm > file.pm.date
       *    (2) building project f.pm returns true, for all f.pm \in file.pm
       *    (3) f.pm.date > file.pm.date, for all f.pm \in file.pm
       * Now, when processing the body of the project the clean flag is preserved until the modification time
       * of a source is different from (newer than) the modification time found in the repository.
       *
       * Second, if the first approach fails we use enrichment to tell if the source actually depends on the 
       * changes.
       *)

      (* The result of building a project is propagated to other projects. We use a project map for this. A 
       * consistent project is one for which it holds that
       *
       *       OS.Path.file absprjid = OS.Path.file absprjid' => absprjid = absprjid',  
       *       for all absolute project identifiers absprjid and absprjid'
       *
       * Consistency checking is implemented in the function `build_project' below.
       *
       *       prjid     ::= name.pm
       *       longprjid ::= prjid | name/longprjid
       *       absprjid  ::= /longprjid
       *)

    (* ----------------------------------------------------
     * Determine where to put target files; if profiling is
     * enabled then we put target files into the PM/Prof/
     * directory; otherwise, we put target files into the
     * PM/NoProf/ directory.
     * ---------------------------------------------------- *)

    fun pmdir() = if !region_profiling then "PM/Prof/" else "PM/NoProf/"

    type absprjid = string

    type projectmap = (prjid * absprjid * extobj list * Basis) list

    fun projectmap_lookup map prjid =
      let fun look [] = NONE
	    | look ((prjid',absprjid,extobjs,basis)::rest) = 
	        if prjid=prjid' then SOME(absprjid,extobjs,basis)
		else look rest
      in look map
      end
      
    fun projectmap_add (prjid, absprjid, extobjs, basis, map) : projectmap =
      (prjid, absprjid, extobjs, basis) :: map

    fun projectmap_plus (projectmap1:projectmap, projectmap2) : projectmap =
      projectmap1 @ projectmap2

    (* Add basislib project if auto import is enabled *)

    fun maybe_add_basislib prjid imports = 
      let val p = !Flags.basislib_project
	  val prjid_basislib = OS.Path.file p
      in if !Flags.auto_import_basislib andalso prjid <> prjid_basislib then p :: imports
	 else imports
      end 
	

    (* Build a project *)

    fun build_project {cycleset : prjid list, pmap : projectmap, longprjid : prjid} 

      : {res_basis : Basis, res_modc : modcode, 
	 pmap : projectmap, extobjs : extobj list, clean : bool} =

      let val {cd_old, file=prjid} = change_dir longprjid
      in let val _ = if member prjid cycleset then
	               error ("There is a cycle in your project; problematic project identifier: " ^ quot prjid)
		     else ()
	     val prj as {imports, extobjs, body} = parse_project prjid
	     val imports = maybe_add_basislib prjid imports
	     val prjid_date_file = pmdir() ^ prjid ^ ".date"
	     val clean = older (OS.FileSys.modTime prjid, OS.FileSys.modTime prjid_date_file) handle _ => false
	     val _ = if clean then () else local_check_project (prjid, prj)
	     val (B, modc, pmap, extobjs, clean) = 
	       foldl(fn (longprjid1,(B, modc, pmap, extobjs, clean0)) => 
		     let val absprjid1 = OS.Path.mkAbsolute(longprjid1, OS.FileSys.getDir())
		         val prjid1 = OS.Path.file longprjid1
		     in case projectmap_lookup pmap prjid1
			  of SOME(absprjid1',extobjs',B') =>
			    if absprjid1 = absprjid1' then (Basis_plus'(B,B'), modc, pmap, extobjs' @ extobjs, clean0)
			    else error ("Your project is inconsistent! The project identifier " ^ quot prjid1 ^ 
					" stands\nfor different projects. Eliminate the inconsistency")
			   | NONE => 
			      let val {res_basis, res_modc, pmap, extobjs=extobjs', clean} = 
				      build_project {cycleset=prjid :: cycleset, pmap=pmap, longprjid=longprjid1}
				  val pmap = projectmap_add(prjid1,absprjid1,extobjs,res_basis,pmap)
			      in (Basis_plus' (B, res_basis), ModCode.seq (modc, res_modc), 
				  pmap, extobjs' @ extobjs, clean0 andalso clean)
			      end
		     end) (Basis.initial(), ModCode.empty, pmap, extobjs, clean) imports

	       (* Now, check that date files associated with imported projects are older than
		* the date file for the current project. *)

	     val clean = foldl (fn (longprjid', clean) => clean andalso
				let val {dir,file} = OS.Path.splitDirFile longprjid'
				    val prjid'_date = OS.Path.concat(dir,pmdir() ^ file ^ ".date")
				in older (OS.FileSys.modTime prjid'_date, OS.FileSys.modTime prjid_date_file) handle _ => false
				end) clean imports

	     val (_, B', modc', clean, modtimes) = build_body (prjid, B, Basis.empty, body, clean, [])
	 in 
	    if clean then () else (maybe_create_PM_dir();
				   output_date_file prjid_date_file);
	    cd_old();
	    {res_basis=B', res_modc=ModCode.seq(modc, modc'), pmap=pmap, extobjs=extobjs, clean=clean}
	 end handle E => (cd_old(); raise E)
      end 

    (* -----------------------------------------
     * build longprjid  builds a project
     * ----------------------------------------- *)

    fun build longprjid =   (* May raise PARSE_ELAB_ERROR *)
      let val _ = Repository.recover()
	  val emitted_files = EqSet.fromList (Repository.emitted_files())
	  val _ = let val {res_modc, clean, extobjs, ...} = build_project{cycleset=[], pmap=[], longprjid=longprjid}

		  (* MEMO: If clean is true then I do not need to rebuild binary. *)

		  in ModCode.mk_exe (OS.Path.file longprjid, res_modc, extobjs, "run")
		  end
	  val emitted_files' = EqSet.fromList (Repository.emitted_files())
    	  val files_to_delete = EqSet.list (EqSet.difference emitted_files emitted_files')
      in List.app ManagerObjects.SystemTools.delete_file files_to_delete
      end

    (* -----------------------------
     * Compile a single file 
     * ----------------------------- *)

    fun comp (filepath : string) : unit =
      let val _ = Repository.recover()
	  val emitted_files = EqSet.fromList (Repository.emitted_files())
	  val prjid = OS.Path.base (OS.Path.file filepath)
	    (* make sure that the source file is indeed recompiled *)
	  val _ = Repository.delete_entries (prjid, ManagerObjects.funid_from_filename filepath)
	  val _ = maybe_create_PM_dir()
	  val (modc_basislib, basis_basislib, extobjs_basislib) = 
	    if !Flags.auto_import_basislib then 
	      let val {res_modc, res_basis, extobjs, ...} = build_project{cycleset=[], pmap=[], longprjid= !Flags.basislib_project}
	      in (res_modc, Basis.plus'(Basis.initial(),res_basis), extobjs)
	      end
	    else (ModCode.empty, Basis.initial(), [])
	  val (_, _, modc_file, _, _) = build_body(prjid, basis_basislib, Basis.empty, UNITbody filepath, false, [])
	  val modc = ModCode.seq(modc_basislib, modc_file)
      in  
	ModCode.mk_exe(prjid, modc, extobjs_basislib, "run")
      end


    (* -----------------------------
     * Elaborate a single file 
     * ----------------------------- *)

    fun elab (unitname : string) : unit =
      let val prjid = unitname
	  val (infB,elabB,_,_) = Basis.un' (Basis.initial())
	  val _ = reset()
	  val log_cleanup = log_init unitname
      in (case ParseElab.parse_elab {prjid=prjid,infB=infB,elabB=elabB,
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
    val _ = Flags.comp_ref := wrap comp

    val interact = Flags.interact

  end
