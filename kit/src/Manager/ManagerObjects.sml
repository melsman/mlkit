(* COMPILER_ENV is the lambda env mapping structure and value 
 * identifiers to lambda env's and lvars *)

(* COMPILE_BASIS is the combined basis of all environments in 
 * the backend *) 

functor ManagerObjects(structure ModuleEnvironments : MODULE_ENVIRONMENTS
		       structure TopdecGrammar : TOPDEC_GRAMMAR   (*needed for type strexp*)
			 sharing type TopdecGrammar.funid = ModuleEnvironments.funid
			 sharing type TopdecGrammar.sigid = ModuleEnvironments.sigid
			 sharing type TopdecGrammar.id = ModuleEnvironments.id
			 sharing type TopdecGrammar.longtycon = ModuleEnvironments.longtycon
			 sharing type TopdecGrammar.longstrid = ModuleEnvironments.longstrid
		       structure OpacityElim : OPACITY_ELIM
			 sharing OpacityElim.TyName = ModuleEnvironments.TyName
			 sharing type OpacityElim.OpacityEnv.realisation = ModuleEnvironments.realisation
			 sharing type OpacityElim.topdec = TopdecGrammar.topdec
		       structure Execution : EXECUTION
			 sharing type Execution.CompilerEnv.id = ModuleEnvironments.id
			 sharing type Execution.CompilerEnv.longid = TopdecGrammar.DecGrammar.Ident.longid
			 sharing type Execution.CompilerEnv.strid = ModuleEnvironments.strid
			 sharing type Execution.CompilerEnv.longstrid = ModuleEnvironments.longstrid
			 sharing type Execution.CompilerEnv.tycon = ModuleEnvironments.tycon
			 sharing type Execution.CompilerEnv.longtycon = ModuleEnvironments.longtycon
			 sharing type Execution.CompileBasis.TyName = ModuleEnvironments.TyName
		       structure Labels : ADDRESS_LABELS
			 sharing type Labels.label = Execution.label
		       structure InfixBasis: INFIX_BASIS
		       structure ElabRep : ELAB_REPOSITORY
			 sharing type ElabRep.funid = TopdecGrammar.funid 
			 sharing type ElabRep.InfixBasis = InfixBasis.Basis
			 sharing type ElabRep.ElabBasis = ModuleEnvironments.Basis
			 sharing type ElabRep.opaq_env = OpacityElim.opaq_env
			 sharing type ElabRep.longstrid = ModuleEnvironments.longstrid
			 sharing ElabRep.TyName = ModuleEnvironments.TyName
                         sharing type ElabRep.absprjid = ModuleEnvironments.absprjid  
		       structure FinMap : FINMAP
		       structure PP : PRETTYPRINT
			 sharing type PP.StringTree = Execution.CompilerEnv.StringTree 
			   = ModuleEnvironments.StringTree = FinMap.StringTree 
			   = InfixBasis.StringTree = OpacityElim.OpacityEnv.StringTree
		       structure Name : NAME
			 sharing type Name.name = ModuleEnvironments.TyName.name = ElabRep.name
		       structure Flags : FLAGS
		       structure Crash : CRASH) : MANAGER_OBJECTS =
  struct

    structure CompilerEnv = Execution.CompilerEnv
    structure CompileBasis = Execution.CompileBasis

    fun die s = Crash.impossible("ManagerObjects." ^ s)
    fun chat s = if !Flags.chat then print (s ^ "\n") else ()

    val link_time_dead_code_elimination = Flags.lookup_flag_entry "link_time_dead_code_elimination"
    local
      val debug_linking = Flags.lookup_flag_entry "debug_linking"
    in
      fun pr_debug_linking s = if !debug_linking then print s else ()
    end

    structure FunId = TopdecGrammar.FunId
    structure SigId = TopdecGrammar.SigId
    structure TyName = ModuleEnvironments.TyName
    type StringTree = PP.StringTree
    type filename = string

    fun mk_filename x = x
    fun filename_to_string x  = x

    type absprjid = ModuleEnvironments.absprjid
(*    type target = Compile.target*)

    type target = Execution.target


    (* ----------------------------------------------------
     * Determine where to put target files; if profiling is
     * enabled then we put target files into the PM/Prof/
     * directory; otherwise, we put target files into the
     * PM/NoProf/ directory.
     * ---------------------------------------------------- *)

    local
      val region_profiling = Flags.lookup_flag_entry "region_profiling"
      val gc_flag          = Flags.lookup_flag_entry "garbage_collection"
    in 
      fun pmdir() = 
	if !region_profiling then 
	  if !gc_flag then
	    "PM/GCProf/" 
	  else 
	    "PM/Prof/"
	else
	  if !gc_flag then
	    "PM/GC/" 
	  else 
	    "PM/NoProf/"
    end

(*
   (* -----------------------------------------------------------------
    * Execute shell command and return the result code.
    * ----------------------------------------------------------------- *)

    structure Shell =
      struct
	exception Execute of string
	fun execute_command command : unit =
	  let fun loop() = 
               OS.Process.system command
	            handle OS.SysErr(s,_) =>
                      (TextIO.output (TextIO.stdOut,
                                      "\ncommand " ^ command ^ 
                        "\nfailed (" ^ s ^ "); \
                         \retry (r), continue (c), or abort (a) ?>");
                        (case TextIO.input1(TextIO.stdIn) of
                           SOME #"r" => loop()
                         | SOME #"c" => OS.Process.success
                         | SOME #"a" => raise Execute ("Exception OS.SysErr \""
				     ^ s ^ "\"\nwhen executing shell command:\n"
			             ^ command)
                         | _ => OS.Process.success
                       )
                      )

                val status = loop()
          in if status <> OS.Process.success then
                       raise Execute ("Error code " ^ Int.toString ~1(*status*) ^
			             " when executing shell command:\n"
			             ^ command)
             else ()
          end
      end
*)

    type linkinfo = Execution.linkinfo
    structure SystemTools =
      struct
(*
	val c_compiler = Flags.lookup_string_entry "c_compiler"
	val c_libs = Flags.lookup_string_entry "c_libs"
*)
	(*logging*)
	val log_to_file = Flags.lookup_flag_entry "log_to_file"

	(*targets*)
	val target_file_extension = Flags.lookup_string_entry "target_file_extension"

	(*linking*)
	val region_profiling = Flags.lookup_flag_entry "region_profiling"
	val gc_flag          = Flags.lookup_flag_entry "garbage_collection"
	fun path_to_runtime () = ! (Flags.lookup_string_entry
				    (if !region_profiling then 
				       if !gc_flag then
					 "path_to_runtime_gc_prof"
				       else
					 "path_to_runtime_prof"
				     else 
				       if !gc_flag then
					 "path_to_runtime_gc"
				       else
					 "path_to_runtime"))


	(* -----------------------------
	 * Append functions
	 * ----------------------------- *)
	  
	fun append_ext s = s ^ !target_file_extension (* changed from ".s" 2000-10-10, Niels *)

	fun append_o s = s ^ ".o"

	(* --------------------
	 * Deleting a file
	 * -------------------- *)

	fun delete_file f = OS.FileSys.remove f handle _ => ()

(*
        (* -----------------------
         * Postponing assembly (to avoid failing system calls)
         *)

        val r : TextIO.outstream option ref  = ref NONE
        fun init_commandfile() = 
            r:= SOME(TextIO.openOut "compile");
        fun add_to_commandfile(s:string) = 
            case !r of NONE => (init_commandfile(); add_to_commandfile s)
            | SOME(os) => TextIO.output(os, s ^"\n")
        fun close_commandfile() = 
            case !r of NONE => ()
            | SOME(os) => TextIO.closeOut os
not used anymore 2000-10-17, Niels *)

	(* -------------------------------
	 * Assemble a file into a .o-file
	 *-------------------------------- *)
(*
	fun assemble (file_s, file_o) =
          (if !(Flags.lookup_flag_entry "delay_assembly")
           then 
	     () (*add_to_commandfile(!c_compiler ^ " -c -o " ^ file_o ^ " " ^ file_s)2000-10-17, Niels*)
           else 
	     (Shell.execute_command (!c_compiler ^ " -c -o " ^ file_o ^ " " ^ file_s);
	      if !(Flags.lookup_flag_entry "delete_target_files")
		then  delete_file file_s 
	      else ()))

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
*)

	val assemble = Execution.assemble

	(* -----------------------------------------------
	 * Emit assembler code and assemble it. 
	 * ----------------------------------------------- *)

	fun emit (target, target_filename) =
	  let val target_filename = pmdir() ^ target_filename
	      val target_filename = OS.Path.mkAbsolute(target_filename, OS.FileSys.getDir())
              val target_filename_s = append_ext target_filename
	      val target_filename_o = append_o target_filename
	      val _ = Execution.emit {target=target,filename=target_filename_s}
	      val _ = assemble (target_filename_s, target_filename_o)
	  in target_filename_o
	  end

	(* -------------------------------------------------------------
	 * Link time dead code elimination; we eliminate all unnecessary
	 * object files from the link sequence before we do the actual
	 * linking. 
	 * ------------------------------------------------------------- *)

	structure labelTable : sig type table
				   val mk : unit -> table
				   val look : table * Labels.label -> bool
				   val insert : table * Labels.label -> unit
			       end =
	  struct
	    type table = (string list) Array.array
	    val table_size = 1009
	    val table_size_word = Word.fromInt table_size
	    fun hash s =
	      let fun loop (0, acc) = acc
		    | loop (i, acc) = loop(i-1, Word.+(Word.*(0w19,acc), 
						       Word.fromInt(Char.ord(String.sub(s,i-1)))))
	      in Word.toInt(Word.mod(loop (String.size s, 0w0), table_size_word))
	      end
	    fun mk () = Array.array (table_size, nil)
	    fun member (a:string) l =
	      let fun f [] = false
		    | f (x::xs) = a=x orelse f xs 
	      in f l
	      end
	    fun look (table,lab) =
	      let val s = Labels.pr_label lab
		  val h = hash s
		  val l = Array.sub(table,h)
	      in member s l
	      end
	    fun insert (table,lab) = 
	      let val s = Labels.pr_label lab
		  val h = hash s
		  val l = Array.sub(table,h)
	      in if member s l then ()
		 else Array.update(table,h,s::l)
	      end
	  end

	fun unsafe(tf,li) = Execution.unsafe_linkinfo li
	fun exports(tf,li) = Execution.exports_of_linkinfo li
	fun imports(tf,li) = Execution.imports_of_linkinfo li
	fun dead_code_elim tfiles_with_linkinfos = 
	  let 
	    val _ = pr_debug_linking "[Link time dead code elimination begin...]\n"
	    val table = labelTable.mk()
	    fun require labs : unit = List.app (fn lab => labelTable.insert(table,lab)) labs
	    fun required labs : bool = foldl (fn (lab,acc) => acc orelse labelTable.look(table,lab)) false labs
	    fun reduce [] = []
	      | reduce (obj::rest) = 
	      let val rest' = reduce rest
		  fun pp_unsafe true = " (unsafe)"
		    | pp_unsafe false = " (safe)"
	      in if unsafe obj orelse required (exports obj) then 
		      (pr_debug_linking ("Using       " ^ #1 obj ^ pp_unsafe(unsafe obj) ^ "\n"); require (imports obj); obj::rest')
		 else (pr_debug_linking ("Discharging " ^ #1 obj ^ "\n"); rest')
	      end
	    val res = reduce tfiles_with_linkinfos
	  in pr_debug_linking "[Link time dead code elimination end...]\n"; res
	  end

	val link_files_with_runtime_system = Execution.link_files_with_runtime_system path_to_runtime
(*
	fun link_files_with_runtime_system files run =
          let 
	    val files = map (fn s => s ^ " ") files
	    val shell_cmd = (!c_compiler ^ " -o " ^ run ^ " " ^ concat files ^ path_to_runtime () ^ " " ^ !c_libs)
	  in
	    (if !(Flags.lookup_flag_entry "delay_assembly") then
	       ((*add_to_commandfile shell_cmd;2000-10-17, Niels*)
		TextIO.output (TextIO.stdOut, "[wrote compile file:\tcompile\n"))
	     else
	       (Shell.execute_command shell_cmd;
	       TextIO.output (TextIO.stdOut, "[wrote executable file:\t" ^ run ^ "]\n")))(*;
	    close_commandfile()2000-10-17, Niels*)
	  end 
	handle Shell.Execute s => die ("link_files_with_runtime_system:\n" ^ s)
*)

	fun member f [] = false
	  | member f ( s :: ss ) = f = s orelse member f ss

	fun elim_dupl ( [] , acc )  = acc
	  | elim_dupl ( f :: fs , acc ) = elim_dupl ( fs, if member f acc then acc else f :: acc )

	(* --------------------------------------------------------------
	 * link (target_files,linkinfos): Produce a link file "link.s". 
	 * Then link the entire project and produce an executable "run".
	 * -------------------------------------------------------------- *)

	fun link (tfiles_with_linkinfos, extobjs, run) : unit =
	  let 
	    val tfiles_with_linkinfos = 
	      if false (*!link_time_dead_code_elimination*) then dead_code_elim tfiles_with_linkinfos
	      else tfiles_with_linkinfos
	    val linkinfos = map #2 tfiles_with_linkinfos
	    val target_files = map #1 tfiles_with_linkinfos
	    val labs = map Execution.code_label_of_linkinfo linkinfos
	    val extobjs = elim_dupl (extobjs,[])
	  in case Execution.generate_link_code
	       of SOME generate_link_code =>
		 let val target_link = generate_link_code labs
		   val linkfile = pmdir() ^ "link_objects"
		   val linkfile_s = append_ext linkfile
		   val linkfile_o = append_o linkfile
		   val _ = Execution.emit {target=target_link, filename=linkfile_s}
		   val _ = assemble (linkfile_s, linkfile_o)
		 in link_files_with_runtime_system (linkfile_o :: (target_files @ extobjs)) run;
		   if !(Flags.lookup_flag_entry "delete_target_files") 
		     andalso  
		     not (!(Flags.lookup_flag_entry "delay_assembly"))
		     then delete_file linkfile_o
		   else ()
		 end
		| NONE => 
		 link_files_with_runtime_system target_files run
	  end
      end (*structure SystemTools*)

    datatype modcode = EMPTY_MODC 
                     | SEQ_MODC of modcode * modcode 
                     | EMITTED_MODC of filename * linkinfo
                     | NOTEMITTED_MODC of target * linkinfo * filename

    structure ModCode =
      struct
	val empty = EMPTY_MODC
	val seq = SEQ_MODC
        fun mk_modcode(t,l,f) = NOTEMITTED_MODC(t,l,f)

	fun exist EMPTY_MODC = true
	  | exist (SEQ_MODC(mc1,mc2)) = exist mc1 andalso exist mc2
	  | exist (NOTEMITTED_MODC _) = true
	  | exist (EMITTED_MODC(file,_)) = OS.FileSys.access (file,[]) handle _ => false

	fun emit(absprjid: absprjid, modc) =
	  let fun em EMPTY_MODC = EMPTY_MODC
		| em (SEQ_MODC(modc1,modc2)) = SEQ_MODC(em modc1, em modc2)
		| em (EMITTED_MODC(fp,li)) = EMITTED_MODC(fp,li)
		| em (NOTEMITTED_MODC(target,linkinfo,filename)) = 
	              EMITTED_MODC(SystemTools.emit(target, OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid)) ^ "-" ^ filename),linkinfo)
                           (*puts ".o" on filename*)
	  in em modc
	  end

	fun mk_exe (absprjid: absprjid, modc, extobjs, run) =
	  let fun get (EMPTY_MODC, acc) = acc
		| get (SEQ_MODC(modc1,modc2), acc) = get(modc1,get(modc2,acc))
		| get (EMITTED_MODC p, acc) = p::acc
		| get (NOTEMITTED_MODC(target,li,filename), acc) =
	             (SystemTools.emit(target, OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid)) ^ "-" ^ filename),li)::acc
	  in SystemTools.link(get(modc,[]), extobjs, run)
	  end

	fun all_emitted modc : bool =
	  case modc
	    of NOTEMITTED_MODC _ => false
	     | SEQ_MODC(mc1,mc2) => all_emitted mc1 andalso all_emitted mc2
	     | _ => true

	fun emitted_files(mc,acc) =
	  case mc
	    of SEQ_MODC(mc1,mc2) => emitted_files(mc1,emitted_files(mc2,acc))
	     | EMITTED_MODC(tfile,_) => tfile::acc
	     | _ => acc   

	fun delete_files (SEQ_MODC(mc1,mc2)) = (delete_files mc1; delete_files mc2)
	  | delete_files (EMITTED_MODC(fp,_)) = SystemTools.delete_file fp
	  | delete_files _ = ()

	fun size mc =
	  case mc
	    of SEQ_MODC(mc1,mc2) => size mc1 +  size mc2
	     | EMPTY_MODC => 0
	     | EMITTED_MODC(tfile,_) => 1
	     | NOTEMITTED_MODC _ => 0

	fun timeStampFileName absprjid =
	  let val base_absprjid = OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid))
	  in "PM/" ^ base_absprjid ^ ".timestamp"
	  end

	val smlserver : bool ref = Flags.lookup_flag_entry "smlserver"

	fun deleteTimeStampFile absprjid : unit =
	  if not(!smlserver) then ()
	  else let val f = timeStampFileName absprjid
	       in OS.FileSys.remove f handle _ => ()
	       end

	fun mk_uoFileList (absprjid: absprjid, modc) : unit =
	  if not(!smlserver) then ()
	  else
	    let val modc = emit (absprjid, modc)
	      val base_absprjid = OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid))
	      fun files_to_be_emitted (mc,acc) =
		case mc
		  of SEQ_MODC(mc1,mc2) => emitted_files(mc1,emitted_files(mc2,acc))
		   | EMITTED_MODC(tfile,_) => tfile::acc
		   | NOTEMITTED_MODC(target,li,filename) =>
		    let val tfile = base_absprjid ^ "-" ^ filename ^ ".o"
		    in tfile::acc
		    end
		   | _ => acc  
	      val files = files_to_be_emitted(modc,nil)
	      fun modify s = case rev (explode s)
			       of #"o" :: rest => implode (rev(#"o" :: #"u" :: rest))
				| _ => s
	      val uofiles = map modify files
	      val ulfile = "PM/" ^ base_absprjid ^ ".ul"
	      val timeStampFile = timeStampFileName(absprjid)
	      val os = TextIO.openOut ulfile
	      val _ = app (fn f => TextIO.output(os, f ^ "\n")) uofiles;
	      val _ = TextIO.closeOut os;
	      val os = TextIO.openOut timeStampFile
	      val _ = TextIO.output(os, "")
	      val _ = TextIO.closeOut os;
	    in	      
	      print("[Created file " ^ ulfile ^ "]\n")
	    end
      end


    (* 
     * Modification times of files
     *)

    type funid = FunId.funid
    fun funid_from_filename (filename: filename) =    (* contains .sml - hence it cannot *)
      FunId.mk_FunId filename                         (* be declared by the user. *)
    fun funid_to_filename (funid: funid) : filename =
      FunId.pr_FunId funid

    datatype funstamp = FUNSTAMP_MODTIME of funid * Time.time
                      | FUNSTAMP_GEN of funid * int
    structure FunStamp =
      struct
	val counter = ref 0
	fun new (funid: funid) : funstamp =
	  FUNSTAMP_GEN (funid, (counter := !counter + 1; !counter))
	fun from_filemodtime (filename: filename) : funstamp option =
	  SOME(FUNSTAMP_MODTIME (funid_from_filename filename, OS.FileSys.modTime filename))
	  handle _ => NONE
	val eq : funstamp * funstamp -> bool = op =
	fun modTime (FUNSTAMP_MODTIME(_,t)) = SOME t
	  | modTime _ = NONE
	fun pr (FUNSTAMP_MODTIME (funid,time)) = FunId.pr_FunId funid ^ "##" ^ Time.toString time
	  | pr (FUNSTAMP_GEN (funid,i)) = FunId.pr_FunId funid ^ "#" ^ Int.toString i
      end

    type ElabEnv = ModuleEnvironments.Env
    type CEnv = CompilerEnv.CEnv
    type CompileBasis = CompileBasis.CompileBasis

    type strexp = TopdecGrammar.strexp
    type strid = ModuleEnvironments.strid

    type sigid = ModuleEnvironments.sigid

    datatype IntSigEnv = ISE of (sigid, TyName.Set.Set) FinMap.map
    datatype IntFunEnv = IFE of (funid, absprjid * funstamp * strid * ElabEnv * (unit -> strexp) * IntBasis) FinMap.map
         and IntBasis = IB of IntFunEnv * IntSigEnv * CEnv * CompileBasis

    (* The closure is to represent a structure expression in a compact way *)

    structure IntFunEnv =
      struct
	val empty = IFE FinMap.empty
	val initial = IFE FinMap.empty
	fun plus(IFE ife1, IFE ife2) = IFE(FinMap.plus(ife1,ife2))
	fun add(funid,e,IFE ife) = IFE(FinMap.add(funid,e,ife))
	fun lookup (IFE ife) funid =
	  case FinMap.lookup ife funid
	    of SOME res => res
	     | NONE => die ("IntFunEnv.lookup: could not find funid " ^ FunId.pr_FunId funid)
	fun restrict (IFE ife, funids) = IFE
	  (foldl (fn (funid, acc) =>
		  case FinMap.lookup ife funid
		    of SOME e => FinMap.add(funid,e,acc)
		     | NONE => die ("IntFunEnv.restrict: could not find funid " ^ FunId.pr_FunId funid)) 
	   FinMap.empty funids)
	fun enrich(IFE ife0, IFE ife) : bool = (* using funstamps; enrichment for free variables is checked *)
	  FinMap.Fold(fn ((funid, obj), b) => b andalso         (* when the functor is being declared!! *)
		      case FinMap.lookup ife0 funid
			of SOME obj0 => FunStamp.eq(#2 obj,#2 obj0) andalso #1 obj = #1 obj0
			 | NONE => false) true ife
	fun layout (IFE ife) = FinMap.layoutMap{start="IntFunEnv = [", eq="->",sep=", ", finish="]"}
	  (PP.LEAF o FunId.pr_FunId) (PP.LEAF o FunStamp.pr o #2) ife
      end


    structure IntSigEnv =
      struct
	val empty = ISE FinMap.empty
	val initial = empty
	fun plus (ISE ise1, ISE ise2) = ISE(FinMap.plus(ise1,ise2))
	fun add (sigid,T,ISE ise) = ISE(FinMap.add(sigid,T,ise))
	fun lookup (ISE ise) sigid =
	  case FinMap.lookup ise sigid
	    of SOME T => T
	     | NONE => die ("IntSigEnv.lookup: could not find sigid " ^ SigId.pr_SigId sigid)
	fun restrict (ISE ise, sigids) = ISE
	  (foldl (fn (sigid, acc) =>
		  case FinMap.lookup ise sigid
		    of SOME e => FinMap.add(sigid,e,acc)
		     | NONE => die ("IntSigEnv.restrict: could not find sigid " ^ SigId.pr_SigId sigid)) 
	   FinMap.empty sigids)
	fun enrich(ISE ise0, ISE ise) : bool = 
	  FinMap.Fold(fn ((sigid, T), b) => b andalso
		      case FinMap.lookup ise0 sigid
			of SOME T0 => TyName.Set.eq T T0
			 | NONE => false) true ise
	fun layout (ISE ise) = FinMap.layoutMap{start="IntSigEnv = [", eq="->",sep=", ", finish="]"}
	  (PP.LEAF o SigId.pr_SigId) 
	  (TyName.Set.layoutSet {start="{",finish="}",sep=", "} (PP.LEAF o TyName.pr_TyName)) ise
	fun tynames (ISE ise) = FinMap.fold (fn (a,b) => TyName.Set.union a b) TyName.Set.empty ise
      end


    type longid = TopdecGrammar.DecGrammar.Ident.longid
    type longstrid = TopdecGrammar.StrId.longstrid
    type longtycon = TopdecGrammar.DecGrammar.TyCon.longtycon
    structure IntBasis =
      struct
	val mk = IB
	fun un (IB ib) = ib
	val empty = IB (IntFunEnv.empty, IntSigEnv.empty, CompilerEnv.emptyCEnv, CompileBasis.empty)
	fun plus (IB(ife1,ise1,ce1,cb1), IB(ife2,ise2,ce2,cb2)) =
	  IB(IntFunEnv.plus(ife1,ife2), IntSigEnv.plus(ise1,ise2), CompilerEnv.plus(ce1,ce2), CompileBasis.plus(cb1,cb2))

	fun restrict (IB(ife,ise,ce,cb), {funids, sigids, longstrids, longvids, longtycons}) =
	    let val ife' = IntFunEnv.restrict(ife,funids)
	        val ise' = IntSigEnv.restrict(ise,sigids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
		(*val _ = if !Flags.chat then (print("\n RESTRICTED CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		val lvars_with_prims = lvars @ (CompilerEnv.primlvarsOfCEnv ce')
		fun tynames_ife(IFE ife, tns) = 
		  let fun tynames_obj ((_,_,_,_,_,obj),tns) = 
		        let val IB(_,ise,ce,_) = obj
			in TyName.Set.list(IntSigEnv.tynames ise) @ (CompilerEnv.tynamesOfCEnv ce @ tns)
			end
		  in FinMap.fold tynames_obj tns ife
		  end
		val tynames = [TyName.tyName_EXN,     (* exn is used explicitly in CompileDec *)
			       TyName.tyName_INT,     (* int needed because of overloading *)
			       TyName.tyName_STRING,  (* string is needed for string constants *)
			       TyName.tyName_REF,
			       TyName.tyName_REAL]    (* real needed because of overloading *)
		  @ (CompilerEnv.tynamesOfCEnv ce')
		val tynames = tynames_ife(ife',tynames)
		val tynames = TyName.Set.list (TyName.Set.union (TyName.Set.fromList tynames) 
					       (IntSigEnv.tynames ise'))
		val cons = CompilerEnv.consOfCEnv ce'
		val excons = CompilerEnv.exconsOfCEnv ce'
		val cb' = CompileBasis.restrict(cb,(lvars,lvars_with_prims,tynames,cons,excons))
	    in IB (ife',ise',ce',cb')
	    end

	fun match(IB(ife1,ise1,ce1,cb1),IB(ife2,ise2,ce2,cb2)) =
	  let val _ = CompilerEnv.match(ce1,ce2)
	      val cb1' = CompileBasis.match(cb1,cb2)
	  in IB(ife1,ise1,ce1,cb1')
	  end

	local 
	  fun IntFunEnv_enrich a = IntFunEnv.enrich a
	  fun IntSigEnv_enrich a = IntSigEnv.enrich a
	  fun CompilerEnv_enrichCEnv a = CompilerEnv.enrichCEnv a
	  fun CompileBasis_enrich a = CompileBasis.enrich a
	  fun CompileBasis_enrich a = CompileBasis.enrich a
	in
	  fun enrich(IB(ife0,ise0,ce0,cb0),IB(ife,ise,ce,cb)) =
	    IntFunEnv_enrich(ife0,ife) andalso IntSigEnv_enrich(ise0,ise) 
	    andalso CompilerEnv_enrichCEnv(ce0,ce) andalso CompileBasis_enrich(cb0,cb)
	end

	local
	  fun agree1(longstrid, (_,_,ce1,cb1), (_,_,ce2,cb2)) =
	    let val ce1 = CompilerEnv.lookup_longstrid ce1 longstrid
	        val ce2 = CompilerEnv.lookup_longstrid ce2 longstrid
	    in
	      CompilerEnv.enrichCEnv(ce1,ce2) andalso CompilerEnv.enrichCEnv(ce2,ce1) andalso
	      let 
		fun restr ce cb =
		  let val lvars = CompilerEnv.lvarsOfCEnv ce
		      val lvars_with_prims = lvars @ (CompilerEnv.primlvarsOfCEnv ce)
		      val tynames = CompilerEnv.tynamesOfCEnv ce
		      val cons = CompilerEnv.consOfCEnv ce
		      val excons = CompilerEnv.exconsOfCEnv ce
		  in CompileBasis.restrict(cb,(lvars,lvars_with_prims,tynames,cons,excons))
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

	fun layout(IB(ife,ise,ce,cb)) =
	  PP.NODE{start="IntBasis = [", finish="]", indent=1, childsep=PP.RIGHT ", ",
		  children=[IntFunEnv.layout ife,
			    IntSigEnv.layout ise,
			    CompilerEnv.layoutCEnv ce,
			    CompileBasis.layout_CompileBasis cb]}
	  
	  (* operations used in Manager, only. *)
	val initial = IB (IntFunEnv.initial, IntSigEnv.initial, CompilerEnv.initialCEnv, CompileBasis.initial)
      end

    type ElabBasis = ModuleEnvironments.Basis 
    type InfixBasis = InfixBasis.Basis
    type opaq_env = OpacityElim.opaq_env     

    datatype Basis = BASIS of InfixBasis * ElabBasis * opaq_env * IntBasis

    structure Basis =
      struct
	val empty = BASIS (InfixBasis.emptyB, ModuleEnvironments.B.empty, OpacityElim.OpacityEnv.empty, IntBasis.empty)
	fun mk b = BASIS b
	fun un (BASIS b) = b
	fun plus (BASIS (infb,elabb,rea,intb), BASIS (infb',elabb',rea',intb')) =
	  BASIS (InfixBasis.compose(infb,infb'), ModuleEnvironments.B.plus (elabb, elabb'),
		 OpacityElim.OpacityEnv.plus(rea,rea'), IntBasis.plus(intb, intb'))

	val debug_man_enrich = Flags.lookup_flag_entry "debug_man_enrich"
	fun log s = TextIO.output(TextIO.stdOut,s)			
	fun debug(s, b) = 
	  if !debug_man_enrich then
	    (if b then log("\n" ^ s ^ ": enrich succeeded.")
	     else log("\n" ^ s ^ ": enrich failed."); b)
	  else b
	local
	  fun InfixBasis_eq a = InfixBasis.eq a
	  fun ModuleEnvironments_B_enrich a = ModuleEnvironments.B.enrich a
	  fun OpacityElim_enrich a = OpacityElim.OpacityEnv.enrich a
	  fun IntBasis_enrich a = IntBasis.enrich a
	in
	  fun enrich (BASIS (infB1,elabB1,rea1,tintB1), (BASIS (infB2,elabB2,rea2,tintB2), dom_rea)) = 
	    debug("InfixBasis", InfixBasis_eq(infB1,infB2)) andalso 
	    debug("ElabBasis", ModuleEnvironments_B_enrich (elabB1,elabB2)) andalso
	    debug("OpacityEnv", OpacityElim_enrich (rea1,(rea2,dom_rea))) andalso
	    debug("IntBasis", IntBasis_enrich(tintB1,tintB2))
	end

	fun agree(longstrids, BASIS(_,elabB1,rea1,tintB1), (BASIS(_,elabB2,rea2,tintB2), dom_rea)) =
	  ModuleEnvironments.B.agree(longstrids,elabB1,elabB2) andalso IntBasis.agree(longstrids,tintB1,tintB2)
	  
	fun layout (BASIS(infB,elabB,rea,intB)) : StringTree =
	  PP.NODE{start="BASIS(", finish = ")",indent=1,childsep=PP.RIGHT ", ",
		  children=[InfixBasis.layoutBasis infB, ModuleEnvironments.B.layout elabB,
			    OpacityElim.OpacityEnv.layout rea, IntBasis.layout intB]}

	val initial = BASIS (InfixBasis.emptyB, ModuleEnvironments.B.initial, OpacityElim.OpacityEnv.initial, IntBasis.initial)
	val _ = app Name.mk_rigid (!Name.bucket)
      end


    type name = Name.name
    structure Repository =
      struct

	type elab_entry = InfixBasis * ElabBasis * longstrid list * (opaq_env * TyName.Set.Set) * 
	  name list * InfixBasis * ElabBasis * opaq_env

	type int_entry = funstamp * ElabEnv * IntBasis * longstrid list * name list * 
	  modcode * IntBasis

	type int_entry' = funstamp * ElabEnv * IntBasis * longstrid list * name list * 
	  modcode * IntBasis

	type intRep = ((absprjid * funid) * bool, int_entry list) FinMap.map ref
	  (* the bool is true if profiling is enabled *)

	type intRep' = ((absprjid * funid) * bool, int_entry' list) FinMap.map ref
	  (* the bool is true if profiling is enabled *)

	val region_profiling : bool ref = Flags.lookup_flag_entry "region_profiling"

	val intRep : intRep = ref FinMap.empty
	val intRep' : intRep' = ref FinMap.empty
	fun clear() = (ElabRep.clear();
		       List.app (List.app (ModCode.delete_files o #6)) (FinMap.range (!intRep));  
		       List.app (List.app (ModCode.delete_files o #6)) (FinMap.range (!intRep'));  
		       intRep := FinMap.empty;
		       intRep' := FinMap.empty)

	val strip_install_dir' = ModuleEnvironments.strip_install_dir'
	val is_absprjid_basislib = ModuleEnvironments.is_absprjid_basislib

	fun delete_rep rep absprjid_and_funid = 
	  case FinMap.remove ((strip_install_dir' absprjid_and_funid, !region_profiling), !rep)
	    of SOME res => rep := res
	     | _ => ()

	fun delete_entries absprjid_and_funid = (ElabRep.delete_entries absprjid_and_funid; 
						 delete_rep intRep absprjid_and_funid;
						 delete_rep intRep' absprjid_and_funid)

	(* To allow the binary distribution of the Kit to be stored in
	  different directories on different systems---to make the Kit
	  relocatable---we must allow the object files for the basis
	  library to be moved to another location after building the
	  system, and still have the object files being reused. To
	  this end, we pass as an argument to the Kit executable the
	  directory in which the Kit is located (installed). The
	  string ref Flags.install_dir is set to this directory during
	  launch of the Kit. *)
	  
	fun prepend_install_dir (funstamp, ElabEnv, IntBasis, longstrids, names, modcode, IntBasis') =
	  let 
	    fun prepend_install_dir_modcode modcode = 
	      case modcode
		of EMPTY_MODC => EMPTY_MODC
		 | SEQ_MODC(modc1,modc2) => SEQ_MODC(prepend_install_dir_modcode modc1, 
						     prepend_install_dir_modcode modc2)
		 | EMITTED_MODC(fp,li) => EMITTED_MODC(OS.Path.concat(!Flags.install_dir,fp),li)
		 | NOTEMITTED_MODC(target,linkinfo,filename) => die "prepend_install_dir_modcode" 
	  in (funstamp, ElabEnv, IntBasis, longstrids, names, prepend_install_dir_modcode modcode, IntBasis')
	  end

	fun remove_install_dir (funstamp, ElabEnv, IntBasis, longstrids, names, modcode, IntBasis') =
	  let 
	    fun remove_install_dir_modcode modcode = 
	      case modcode
		of EMPTY_MODC => EMPTY_MODC
		 | SEQ_MODC(modc1,modc2) => SEQ_MODC(remove_install_dir_modcode modc1, 
						     remove_install_dir_modcode modc2)
		 | EMITTED_MODC(fp,li) => EMITTED_MODC(OS.Path.mkRelative(fp, !Flags.install_dir),li)
		 | NOTEMITTED_MODC(target,linkinfo,filename) => die "remove_install_dir_modcode" 
	  in (funstamp, ElabEnv, IntBasis, longstrids, names, remove_install_dir_modcode modcode, IntBasis')
	  end

	fun lookup_rep rep exportnames_from_entry (absprjid_and_funid as (absprjid,_)) =
	  let val all_gen = foldl (fn (n, b) => b andalso Name.is_gen n) true
	      fun find ([], n) = NONE
		| find (entry::entries, n) = 
		if (all_gen o exportnames_from_entry) entry then 
		  (* if absprjid is "basislib.pm" then we prepend to the entry the 
		   * directory in which the o.-files are located (the install_dir). *)
		  if is_absprjid_basislib absprjid then
		    SOME(n, prepend_install_dir entry)
		  else SOME(n,entry)
		else find(entries,n+1)
	  in case FinMap.lookup (!rep) (strip_install_dir' absprjid_and_funid, !region_profiling)
	       of SOME entries => find(entries, 0)
		| NONE => NONE
	  end

	fun add_rep rep (absprjid_and_funid as (absprjid,_),entry) : unit =
	  rep := let val r = !rep 
		     val i = (strip_install_dir' absprjid_and_funid, !region_profiling)
		 in case FinMap.lookup r i
		      of SOME res => FinMap.add(i,res @ [entry],r)
		       | NONE => FinMap.add(i, [if is_absprjid_basislib absprjid then
						  remove_install_dir entry
						else entry], r)
		 end

	fun owr_rep rep (absprjid_and_funid,n,entry) : unit =
	  rep := let val r = !rep
		     val i = (strip_install_dir' absprjid_and_funid, !region_profiling)
	             fun owr(0,entry::res,entry') = entry'::res
		       | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		       | owr _ = die "owr_rep.owr"
		 in case FinMap.lookup r i
		      of SOME res => FinMap.add(i,owr(n,res,entry),r)
		       | NONE => die "owr_rep.NONE"
		 end
	val lookup_int = lookup_rep intRep #5
	val lookup_int' = lookup_rep intRep' #5

	fun add_int (absprjid_and_funid,entry) = 
	  if ModCode.all_emitted (#6 entry) then  (* just make sure... *)
	    add_rep intRep (absprjid_and_funid, entry)
	  else die "add_int"

	fun add_int' (absprjid_and_funid,entry) = 
	  if ModCode.all_emitted (#6 entry) then  (* just make sure... *)
	    add_rep intRep' (absprjid_and_funid, entry)
	  else die "add_int'"

	fun owr_int (absprjid_and_funid,n,entry) =
	  if ModCode.all_emitted (#6 entry) then  (* just make sure... *)
	    owr_rep intRep (absprjid_and_funid,n,entry)
	  else die "owr_int"

	fun recover_intrep ir =
	  List.app 
	  (List.app (fn entry : 'a1*'a2*'a3*'a4*(name list)*'a6*'a7 => List.app Name.mark_gen (#5 entry)))
	  (FinMap.range ir)

	fun emitted_files() =
	  let fun files_entries ([],acc) = acc
		| files_entries ((_,_,_,_,_,mc,_)::entries,acc) = 
		    files_entries(entries,ModCode.emitted_files(mc,acc))
	  in FinMap.fold files_entries (FinMap.fold files_entries [] (!intRep)) (!intRep')
	  end
	val lookup_elab = ElabRep.lookup_elab
	val add_elab = ElabRep.add_elab
	val owr_elab = ElabRep.owr_elab
	fun recover() = (ElabRep.recover(); recover_intrep (!intRep); recover_intrep (!intRep'))
      end
    
  end
