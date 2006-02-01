(* COMPILER_ENV is the lambda env mapping structure and value 
 * identifiers to lambda env's and lvars *)

(* COMPILE_BASIS is the combined basis of all environments in 
 * the backend *) 

functor ManagerObjects(Execution : EXECUTION) : MANAGER_OBJECTS =
  struct
    structure PP = PrettyPrint
    structure TopdecGrammar = PostElabTopdecGrammar
    structure CompileBasis = Execution.CompileBasis
    structure Labels = AddressLabels

    structure ErrorCode = ParseElab.ErrorCode
    exception PARSE_ELAB_ERROR of ErrorCode.ErrorCode list
    fun error (s : string) = (print ("\nError: " ^ s ^ ".\n\n"); raise PARSE_ELAB_ERROR[])
    fun warn (s : string) = print ("\nWarning: " ^ s ^ ".\n\n")
    fun quot s = "`" ^ s ^ "'"
    val op ## = OS.Path.concat infix ##

    val backend_name = Execution.backend_name
    val compile_only = Flags.is_on0 "compile_only"

    fun die s = Crash.impossible("ManagerObjects." ^ s)
    fun chat s = if !Flags.chat then print (s ^ "\n") else ()

    val link_time_dead_code_elimination = 
	Flags.add_bool_entry {long="link_time_dead_code_elimination", short=SOME "ltdce", item=ref true,
			      menu=["Control", "link time dead code elimination"], neg=true,
			      desc="Link time dead code elimination."}
    local
      val debug_linking = Flags.lookup_flag_entry "debug_linking"
    in
      fun pr_debug_linking s = if !debug_linking then print s else ()
    end

    type StringTree = PP.StringTree
    type filename = string

    fun mk_filename x = x
    fun filename_to_string x  = x

    type absprjid = ModuleEnvironments.absprjid

    type target = Execution.target

    val gc_p = Flags.is_on0 "garbage_collection"
    val gengc_p = Flags.is_on0 "generational_garbage_collection"

    (* ----------------------------------------------------
     * Determine where to put target files
     * ---------------------------------------------------- *)

    local
      val region_profiling = Flags.is_on0 "region_profiling"
      val recompile_basislib = Flags.is_on0 "recompile_basislib"
      val tag_pairs_p = Flags.is_on0 "tag_pairs"
    in 
	(* Remember also to update RepositoryFinMap in Common/Elaboration.sml *)
      fun mlbdir() = 
	  let val subdir =
	      if !Flags.SMLserver then "SMLserver"
	      else if recompile_basislib() then "Scratch"   (* avoid overwriting other files *)
	      else 
		  case (gengc_p(),gc_p(), region_profiling(), tag_pairs_p()) of 
		      (false,     true,   true,               false) => "RI_GC_PROF"
		    | (false,     true,   false,              false) => "RI_GC"
		    | (false,     true,   true,               true)  => "RI_GC_TP_PROF"
		    | (false,     true,   false,              true)  => "RI_GC_TP"
		    | (true,      true,   true,               false) => "RI_GEN_GC_PROF"
		    | (true,      true,   false,              false) => "RI_GEN_GC"
		    | (true,      _,      _,                  _)     => die "Illegal combination of generational garbage collection and tagged pairs"
		    | (false,     false,  true,               _)     => "RI_PROF"
		    | (false,     false,  false,              _)     => "RI"
	  in "MLB" ## subdir
	  end
    end

    type linkinfo = Execution.linkinfo
    structure SystemTools =
      struct
	(*logging*)
	val log_to_file = Flags.lookup_flag_entry "log_to_file"

	(*linking*)
	val region_profiling = Flags.lookup_flag_entry "region_profiling"

	val tag_values = Flags.is_on0 "tag_values"

	val tag_pairs_p = Flags.is_on0 "tag_pairs"

	fun path_to_runtime () = 
	  let fun file () = 
	      if !region_profiling andalso gc_p() andalso tag_pairs_p() then "runtimeSystemGCTPProf.a"  else
	      if !region_profiling andalso gc_p() andalso gengc_p()     then "runtimeSystemGenGCProf.a" else
	      if !region_profiling andalso gc_p()                       then "runtimeSystemGCProf.a"    else
	      if !region_profiling                                      then "runtimeSystemProf.a"      else
              if                           gc_p() andalso tag_pairs_p() then "runtimeSystemGCTP.a"      else
              if                           gc_p() andalso gengc_p()     then "runtimeSystemGenGC.a"     else
              if                           gc_p()                       then "runtimeSystemGC.a"        else
              if tag_values()                     andalso tag_pairs_p() then 
		  die "no runtime system supports tagging of values with tagging of pairs"             else
              if tag_values()                                           then "runtimeSystemTag.a"      else
		                                                             "runtimeSystem.a"
	  in !Flags.install_dir ## "lib" ## file()
	  end

      	(* --------------------
	 * Deleting a file
	 * -------------------- *)

	fun delete_file f = OS.FileSys.remove f handle _ => ()


	(* --------------------
	 * Changing directories
	 * 
	 * -------------------- *)
(*
	fun change_dir p : {cd_old : unit -> unit, file : string} =
	    let val {dir,file} = OS.Path.splitDirFile p
	    in if dir = "" then {cd_old = fn()=>(),file=file}
	       else let val old_dir = OS.FileSys.getDir()
			val _ = OS.FileSys.chDir dir
		    in {cd_old=fn()=>(OS.FileSys.chDir old_dir), file=file}
		    end handle OS.SysErr _ => error ("I cannot access directory " ^ quot dir)
	    end
	(* [change_dir p] cd's to the dir part of p and returns the
	 * file part of p together with a function for changing to the
	 * original directory. *)
*)	
	(* -----------------------------------------------
	 * Creating directories for target code
	 * ----------------------------------------------- *)

	fun maybe_create_dir d : unit = 
	    if OS.FileSys.access (d, []) handle _ => error ("I cannot access directory " ^ quot d) then
		if OS.FileSys.isDir d then ()
		else error ("The file " ^ quot d ^ " is not a directory")
	    else ((OS.FileSys.mkDir d;()) handle _ => 
		  error ("I cannot create directory " ^ quot d ^ " --- the current directory is " ^ 
			 OS.FileSys.getDir()))
		
	fun maybe_create_dirs {prepath:string,dirs:string} : unit =
	    let val dirs = String.tokens (fn c => c = #"/") dirs
		fun loop (p, nil) = ()
		  | loop (p, d::ds) = let val p = p ## d
				      in maybe_create_dir p; loop(p, ds)
				      end
	    in loop(prepath, dirs)
	    end

	fun maybe_create_mlbdir {prepath:string} : unit = 
	    maybe_create_dirs {prepath=prepath,dirs=mlbdir()}


	(* -----------------------------------------------
	 * Emit assembler code and assemble it. 
	 * ----------------------------------------------- *)

	fun emit (target,absprjid,filename) =
	    let fun esc n = 
		  let fun loop nil acc = implode(rev acc)
                        | loop (#"." :: #"." :: cc) acc = loop cc (#"%"::acc)
                        | loop (#"/" :: cc) acc = loop cc (#"+"::acc)
                        | loop (c :: cc) acc = loop cc (c::acc)
		  in loop (explode n) nil
		  end
		val target_filename =
		   if Flags.is_on "compile_only" then
		       let val p = OS.Path.base(Flags.get_string_entry "output") 
			   val filename = OS.Path.file filename
		       in if OS.Path.file p = filename then p else p ^ "." ^ filename
		       end
		   else
(*
		       let 
			   val target_filename = OS.Path.base(OS.Path.file absprjid) ^ "-" ^ esc filename
			   val target_filename = pmdir() ^ target_filename
		       in OS.Path.mkAbsolute{path=target_filename, relativeTo=OS.FileSys.getDir()}
		       end
*)
		       let val {dir,file} = OS.Path.splitDirFile filename
			   val target_filename = OS.Path.base(OS.Path.file absprjid) ^ "-" ^ file
			   val target_filename = mlbdir() ## target_filename
			   val _ = maybe_create_mlbdir {prepath=dir}
		       in dir ## target_filename
		       end
		   val f0 = Execution.emit {target=target,filename=target_filename}
		   val f = OS.Path.file f0
	    in mlbdir() ## f (* strip the initial dir in the recorded code file *)
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
	    val allexports = labelTable.mk()
	    fun require (f_labs,d_labs) : unit = (List.app (fn lab => labelTable.insert(table,lab)) f_labs;
						  List.app (fn lab => labelTable.insert(table,lab)) d_labs) (* 2001-01-09, Niels *)
	    fun add_exports_to_allexports (f_labs,d_labs) =
		let fun look l = 
			if labelTable.look(allexports, l) then
			    die ("Label " ^ Labels.pr_label l ^ " allready exported")
			else ()
		in
		    (List.app (fn lab => (look lab ; labelTable.insert(allexports,lab))) f_labs;
		     List.app (fn lab => (look lab ; labelTable.insert(allexports,lab))) d_labs)		
		end
		
	    fun required (f_labs,d_labs) : bool = 
	      foldl (fn (lab,acc) => acc orelse labelTable.look(table,lab))
	      (foldl (fn (lab,acc) => acc orelse labelTable.look(table,lab)) false f_labs) d_labs  (* 2001-01-09, Niels *)
	    fun reduce [] = []
	      | reduce (obj::rest) = 
	      let val rest' = reduce rest
		  fun pp_unsafe true = " (unsafe)"
		    | pp_unsafe false = " (safe)"
	      in if unsafe obj orelse required (exports obj) then 
		     (pr_debug_linking ("Using       " ^ #1 obj ^ pp_unsafe(unsafe obj) ^ "\n")
		      ; require (imports obj)
		      ; add_exports_to_allexports (exports obj)
		      ; obj::rest')
		 else (pr_debug_linking ("Discharging " ^ #1 obj ^ "\n"); rest')
	      end
	    val res = reduce tfiles_with_linkinfos
	  in pr_debug_linking "[Link time dead code elimination end...]\n"; res
	  end

	val link_files_with_runtime_system = Execution.link_files_with_runtime_system path_to_runtime

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
	      if link_time_dead_code_elimination() then dead_code_elim tfiles_with_linkinfos
	      else tfiles_with_linkinfos
	    val linkinfos = map #2 tfiles_with_linkinfos
	    val target_files = map #1 tfiles_with_linkinfos
	    val labs = map Execution.code_label_of_linkinfo linkinfos
	    val exports = 
	      List.foldr (fn ((fs,ds),(acc_f,acc_d)) =>  (fs@acc_f, ds@acc_d)) ([],[]) 
	      (map Execution.exports_of_linkinfo linkinfos)  (* 2001-01-09, Niels *)
	    val extobjs = elim_dupl (extobjs,[])
	  in case Execution.generate_link_code
	       of SOME generate_link_code =>
		 let val target_link = generate_link_code (labs, exports)
		   val linkfile_o = emit(target_link, "base", "link_objects")
		 in link_files_with_runtime_system (linkfile_o :: (target_files @ extobjs)) run;
		     delete_file linkfile_o
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
	  | exist (EMITTED_MODC(file,_)) = 
	  let val res = OS.FileSys.access (file,[]) handle _ => false
	  in if res then res
	     else (print ("File " ^ file ^ " not present\n"); res)
	  end

	fun emit(absprjid: absprjid, modc) =
	  let 
	    fun em EMPTY_MODC = EMPTY_MODC
	      | em (SEQ_MODC(modc1,modc2)) = SEQ_MODC(em modc1, em modc2)
	      | em (EMITTED_MODC(fp,li)) = EMITTED_MODC(fp,li)
	      | em (NOTEMITTED_MODC(target,linkinfo,filename)) = 
	      EMITTED_MODC(SystemTools.emit(target,ModuleEnvironments.absprjid_to_string absprjid,filename),linkinfo)
                           (*puts ".o" on filename*)
	  in em modc
	  end

	fun mk_exe (absprjid: absprjid, modc, extobjs, run) =
	    if compile_only() then ()
	    else
		let fun get (EMPTY_MODC, acc) = acc
		      | get (SEQ_MODC(modc1,modc2), acc) = get(modc1,get(modc2,acc))
		      | get (EMITTED_MODC p, acc) = p::acc
		      | get (NOTEMITTED_MODC(target,li,filename), acc) =
		    (SystemTools.emit(target,ModuleEnvironments.absprjid_to_string absprjid,filename),li)::acc
		in SystemTools.link(get(modc,[]), extobjs, run)
		end

	fun mk_exe_all_emitted (modc, extobjs, run) =
	    if compile_only() then ()
	    else
		let fun get (EMPTY_MODC, acc) = acc
		      | get (SEQ_MODC(modc1,modc2), acc) = get(modc1,get(modc2,acc))
		      | get (EMITTED_MODC p, acc) = p::acc
		      | get (NOTEMITTED_MODC(target,li,filename), acc) = die "mk_exe_all_emitted"
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
(*
	fun timeStampFileName absprjid =
	  let val base_absprjid = OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid))
	  in "PM/" ^ base_absprjid ^ ".timestamp"
	  end
*)
	fun ulfile (absprjid: absprjid) : string =
	    let val base_absprjid = OS.Path.base(OS.Path.file(ModuleEnvironments.absprjid_to_string absprjid))
	    in "PM/" ^ base_absprjid ^ ".ul"
	    end

	fun deleteUlfile absprjid : unit =
	  if not(!Flags.SMLserver) then ()
	  else let val f = ulfile absprjid
	       in OS.FileSys.remove f handle _ => ()
	       end

	fun list_minus (xs,nil) = xs
	  | list_minus (x::xs,y::ys) = 
	    if x = y then list_minus(xs,ys)
	    else die "list_minus.prefix error1"
	  | list_minus _ = die "list_minus.prefix error2"

	fun target_files modc : string list =
	    let fun files (mc,acc) =
		case mc of 
		    SEQ_MODC(mc1,mc2) => files(mc1,files(mc2,acc))
		  | EMITTED_MODC(tfile,_) => tfile::acc
		  | NOTEMITTED_MODC(target,li,filename) => die "target_files: file not emitted"
		  | _ => acc  
	    in files(modc,nil)
	    end	    

	fun makeUlfile (ulfile: string, modc, modc') : unit =
	  if not(!Flags.SMLserver) then ()
	  else
	      (* modc is a prefix of modc' *)
	    let 
		val _ = 
		    if not (all_emitted modc) orelse not(all_emitted modc') then
			die "makeUlfile: not all emitted"
		    else ()
(*		val modc = emit (absprjid, modc') *)
	      val uofiles_local = target_files modc
	      val uofiles_local_and_scripts = target_files modc'
	      val uofiles_scripts = list_minus(uofiles_local_and_scripts,uofiles_local)
(*	      val uofiles_scripts = map OS.Path.file uofiles_scripts *)
	      val _ = 
		  let val os = TextIO.openOut ulfile
		  in    app (fn f => TextIO.output(os, f ^ "\n")) uofiles_local
		      ; TextIO.output(os, "scripts:\n")
		      ; app (fn f => TextIO.output(os, f ^ "\n")) uofiles_scripts
		      ; TextIO.closeOut os
		  end
(*
	      val timeStampFile = timeStampFileName absprjid
	      val os = TextIO.openOut timeStampFile
	      val _ = TextIO.output(os, "")
	      val _ = TextIO.closeOut os;
*)
	    in	      
	      print("[wrote file " ^ ulfile ^ "]\n")
	    end

	val pu =
	    let fun toInt EMPTY_MODC = 0
		  | toInt (SEQ_MODC _) = 1
		  | toInt (EMITTED_MODC _) = 2
		  | toInt (NOTEMITTED_MODC _) = 3
		val fun_EMPTY_MODC = Pickle.con0 EMPTY_MODC
		fun fun_SEQ_MODC pu = 
		    Pickle.con1 SEQ_MODC (fn SEQ_MODC a => a | _ => die "ModCode.pu.SEQ_MODC")
		    (Pickle.pairGen(pu,pu))
		fun fun_EMITTED_MODC _ = 
		    Pickle.con1 EMITTED_MODC (fn EMITTED_MODC a => a | _ => die "ModCode.pu.EMITTED_MODC")
		    (Pickle.pairGen(Pickle.string,Execution.pu_linkinfo))
		fun error _ = die "ModCode.pu.NOTEMITTED_MODC"
		fun fun_NOTEMITTED_MODC _ = 
		    Pickle.con1 error error (Pickle.convert (error,error) Pickle.unit)
	    in Pickle.dataGen("ModCode",toInt,[fun_EMPTY_MODC,
					       fun_SEQ_MODC,
					       fun_EMITTED_MODC,
					       fun_NOTEMITTED_MODC])
	    end

	fun dirMod0 f EMPTY_MODC = EMPTY_MODC
	  | dirMod0 f (SEQ_MODC(modc1,modc2)) = SEQ_MODC(dirMod0 f modc1, dirMod0 f modc2)
	  | dirMod0 f (EMITTED_MODC(fp,li)) = EMITTED_MODC(f fp,li)
	  | dirMod0 f (NOTEMITTED_MODC(t,li,fp)) = NOTEMITTED_MODC(t,li,f fp)

	fun subtract_mlbdir s =
	    (OS.Path.getParent o OS.Path.getParent) s

	fun dirMod d m =
	    dirMod0 (fn fp => 
		     let val p = d ## OS.Path.file fp
			 val p = 
			     if OS.Path.isAbsolute d then p
			     else subtract_mlbdir(OS.Path.dir fp) ## p
		     in p
		     end) m

	fun absDirMod absd m =
	    dirMod0 (fn fp => absd ## OS.Path.file fp) m
	    
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
	val pu = 
	    let val pu_funid_time = Pickle.pairGen(FunId.pu,Pickle.time)
		val pu_funid_int = Pickle.pairGen(FunId.pu,Pickle.int)
		fun toInt (FUNSTAMP_MODTIME _) = 0
		  | toInt (FUNSTAMP_GEN _) = 1
		fun fun_FUNSTAMP_MODTIME _ =
		    Pickle.con1 FUNSTAMP_MODTIME (fn FUNSTAMP_MODTIME v => v | _ => die "pu.FUNSTAMP_MODTIME")
		    pu_funid_time
		fun fun_FUNSTAMP_GEN _ =
		    Pickle.con1 FUNSTAMP_GEN (fn FUNSTAMP_GEN v => v | _ => die "pu.FUNSTAMP_GEN")
		    pu_funid_int
	    in Pickle.dataGen("FunStamp",toInt,[fun_FUNSTAMP_MODTIME, fun_FUNSTAMP_GEN])
	    end
      end

    type ElabEnv = ModuleEnvironments.Env
    type CEnv = CompilerEnv.CEnv
    type CompileBasis = CompileBasis.CompileBasis

    type strexp = TopdecGrammar.strexp
    type strid = ModuleEnvironments.strid

    type sigid = ModuleEnvironments.sigid

    type ElabBasis = ModuleEnvironments.Basis 
    type InfixBasis = InfixBasis.Basis
    type opaq_env = OpacityElim.opaq_env     

    type md5 = string
    type BodyBuilderClos = {infB: InfixBasis,
			    elabB: ElabBasis,
			    absprjid: absprjid,
			    filename: string,
			    filemd5: md5,
			    opaq_env: opaq_env,
			    T: TyName.TyName list,
			    resE: ElabEnv}

    datatype IntSigEnv = ISE of (sigid, TyName.Set.Set) FinMap.map
    datatype IntFunEnv = IFE of (funid, absprjid * funstamp * strid * ElabEnv * BodyBuilderClos * IntBasis) FinMap.map
         and IntBasis = IB of IntFunEnv * IntSigEnv * CEnv * CompileBasis

    (* Instead of storing structure expressions in functor environments, information necessary for recreating 
     * structure expressions is stored (BodyBuilderClos). *)

    local
	fun layoutBBC bbc =
	    PP.NODE{start="BBC=<todo",finish=">", indent=2,
		    childsep=PP.RIGHT", ", children=[]}
    in
	fun layoutIntSigEnv (ISE ise) = FinMap.layoutMap{start="IntSigEnv = [", eq="->",sep=", ", finish="]"}
	    (PP.LEAF o SigId.pr_SigId) 
	  (TyName.Set.layoutSet {start="{",finish="}",sep=", "} (PP.LEAF o TyName.pr_TyName)) ise

	fun layoutIntBasis(IB(ife,ise,ce,cb)) =
	    PP.NODE{start="IntBasis = [", finish="]", indent=1, childsep=PP.RIGHT ", ",
		    children=[layoutIntFunEnv ife,
			      layoutIntSigEnv ise,
			      CompilerEnv.layoutCEnv ce,
			      CompileBasis.layout_CompileBasis cb]}
	and layoutClos a = 
	    let val (absprjid, funstamp, strid, E, BBC, ib) = a
	    in PP.NODE{start="<", finish=">",
		       childsep=PP.RIGHT", ", indent=2,
		       children=[PP.LEAF ("funstamp=" ^ FunStamp.pr funstamp),
				 layoutBBC BBC,
				 layoutIntBasis ib]}				 
	    end
	and layoutIntFunEnv (IFE ife) = FinMap.layoutMap{start="IntFunEnv = [", eq="->",sep=", ", finish="]"}
	    (PP.LEAF o FunId.pr_FunId) layoutClos ife

    end

    (* Picklers *)

    val pu_BodyBuilderClos =
	let fun to ((infB,elabB,absprjid),(filename,opaq_env,T),(resE,filemd5)) = 
		{infB=infB,elabB=elabB,absprjid=absprjid,filename=filename,filemd5=filemd5,
		 opaq_env=opaq_env,T=T,resE=resE}
	    fun from {infB=infB,elabB=elabB,absprjid=absprjid,filename=filename,filemd5=filemd5,
		      opaq_env=opaq_env,T=T,resE=resE} = ((infB,elabB,absprjid),(filename,opaq_env,T),(resE,filemd5))
	in Pickle.convert (to,from)
	    (Pickle.tup3Gen0(Pickle.tup3Gen0(InfixBasis.pu,ModuleEnvironments.B.pu,ModuleEnvironments.pu_absprjid),
			     Pickle.tup3Gen0(Pickle.string,OpacityEnv.pu,Pickle.listGen TyName.pu), 
			     Pickle.pairGen0(Environments.E.pu,Pickle.string)))
	end

    val pu_IntSigEnv =
	Pickle.convert (ISE, fn ISE v => v) 
	(FinMap.pu(SigId.pu,TyName.Set.pu TyName.pu))

    val (pu_IntFunEnv,pu_IntBasis) =
	let fun IntFunEnvToInt _ = 0
	    fun IntBasisToInt _ = 0
	    fun fun_IFE (pu_IntFunEnv, pu_IntBasis) =
		Pickle.con1 IFE (fn IFE a => a)		
		(FinMap.pu (FunId.pu,
			    Pickle.convert (fn ((a,b,c),(d,e,f)) => (a,b,c,d,e,f), fn (a,b,c,d,e,f) => ((a,b,c),(d,e,f)))
			    (Pickle.pairGen0(Pickle.tup3Gen0(ModuleEnvironments.pu_absprjid,FunStamp.pu,StrId.pu),
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
			of SOME obj0 => 
			            FunStamp.eq(#2 obj,#2 obj0)            (* modtime *)
			    andalso #1 obj = #1 obj0                       (* absprjid *)
			    andalso #filemd5 (#5 obj) = #filemd5 (#5 obj0) (* md5 of functor body *)
			 | NONE => false) true ife

	val layout = layoutIntFunEnv
	fun fold f i (IFE ife) = FinMap.Fold f i ife
	val pu = pu_IntFunEnv
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
	val layout = layoutIntSigEnv
	fun tynames (ISE ise) = FinMap.fold (fn (a,b) => TyName.Set.union a b) TyName.Set.empty ise

	val pu = pu_IntSigEnv
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

	fun restrict0 (IB(ife,ise,ce,cb), {funids, sigids, longstrids, longvids, longtycons}, tynames) =
	    let val ife' = IntFunEnv.restrict(ife,funids)
	        val ise' = IntSigEnv.restrict(ise,sigids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
		(*val _ = if !Flags.chat then (print("\n RESTRICTED CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		fun tynames_ife(IFE ife) = 
		  let fun tynames_obj ((_,_,_,_,_,obj),tns) = 
		        let val IB(_,ise,ce,_) = obj
			in TyName.Set.list(IntSigEnv.tynames ise) @ (CompilerEnv.tynamesOfCEnv ce @ tns)
			end
		  in FinMap.fold tynames_obj nil ife
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
			       TyName.tyName_INT31,   (* int31, int32, intinf, word8, word31, word32 needed *)
			       TyName.tyName_INT32,   (*     because of overloading *)
			       TyName.tyName_INTINF,
			       TyName.tyName_WORD8,
			       TyName.tyName_WORD31,
			       TyName.tyName_WORD32,
			       TyName.tyName_STRING,  (* string is needed for string constants *)
			       TyName.tyName_CHAR,    (* char is needed for char constants *)
			       TyName.tyName_REF,
			       TyName.tyName_REAL]    (* real needed because of overloading *)
		  @ TyName.Set.list tynames
		val IB(ife,ise,ce,cb) = ib
		val {funids, sigids, longstrids, longvids, longtycons} = ids
		val longstrids = StrId.mk_LongStrId ["IntInfRep"] :: longstrids
		val ife' = IntFunEnv.restrict(ife,funids)
	        val ise' = IntSigEnv.restrict(ise,sigids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
		(*val _ = if !Flags.chat then (print("\n RESTRICTED CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		fun tynames_ife(IFE ife) = 
		  let fun tynames_obj ((_,_,_,_,_,obj),tns) = 
		        let val IB(_,ise,ce,_) = obj
			in TyName.Set.list(IntSigEnv.tynames ise) @ (CompilerEnv.tynamesOfCEnv ce @ tns)
			end
		  in FinMap.fold tynames_obj nil ife
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
					   let val IB ib = #6 functorClos
					   in CompileBasis.plus(cb,#4 ib)
					   end) cb' ife'

	    in IB (ife',ise',ce',cb'')
	    end

	fun match(IB(ife1,ise1,ce1,cb1),IB(ife2,ise2,ce2,cb2)) =
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
	fun initial() = IB (IntFunEnv.initial, IntSigEnv.initial, 
			    CompilerEnv.initialCEnv(), CompileBasis.initial)
	val pu = pu_IntBasis

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
	fun debug(s, b) = 
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

	fun agree(longstrids, BASIS(_,elabB1,rea1,tintB1), (BASIS(_,elabB2,rea2,tintB2), dom_rea)) =
	  ModuleEnvironments.B.agree(longstrids,elabB1,elabB2) andalso IntBasis.agree(longstrids,tintB1,tintB2)

	fun restrict(BASIS(infB,eB,oe,iB),ids: longids) =
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

	fun domain(BASIS(_,eB,_,_)) : longids = ModuleEnvironments.B.domain eB 

	fun db_f s true = true
	  | db_f s false = false before print ("Basis.eq:" ^ s ^ " false\n")

	fun eq(BASIS(infB1,eB1,oe1,iB1), BASIS(infB2,eB2,oe2,iB2)) =
	    db_f "InfixBasis" (InfixBasis.eq(infB1,infB2)) andalso 
	    db_f "B_l" (ModuleEnvironments.B.enrich(eB1,eB2)) andalso db_f "B_r" (ModuleEnvironments.B.enrich(eB2,eB1)) andalso
	    db_f "OpacityEnv" (OpacityEnv.eq(oe1,oe2)) andalso 
	    db_f "IB_l" (IntBasis.enrich(iB1,iB2)) andalso db_f "IB_r" (IntBasis.enrich(iB2,iB1))

	fun closure (B': Basis, B: Basis) : Basis = 
	    (* closure_B'(B) : the closure of B w.r.t. B' *)
(*was:
	    let val dom = domain B
	    in #1 (restrict(plus(B',B),dom))
	    end
*)
	    let val BASIS(infB',eB',oe',iB') = B'
		val BASIS(infB,eB,oe,iB) = B
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
	  
	fun layout (BASIS(infB,elabB,rea,intB)) : StringTree =
	  PP.NODE{start="BASIS(", finish = ")",indent=1,childsep=PP.RIGHT ", ",
		  children=[InfixBasis.layoutBasis infB, ModuleEnvironments.B.layout elabB,
			    OpacityEnv.layout rea, IntBasis.layout intB]}

	fun initial() = BASIS (InfixBasis.emptyB, 
			       ModuleEnvironments.B.initial(), 
			       OpacityEnv.initial, 
			       IntBasis.initial())
	val _ = app Name.mk_rigid (!Name.bucket)

	val pu =
	    Pickle.comment "MO.Basis"
	    (Pickle.convert (BASIS, fn BASIS a => a)
	     (Pickle.tup4Gen0(InfixBasis.pu, 
			      Pickle.comment "ModuleEnvironments.B.pu" ModuleEnvironments.B.pu, 
			      Pickle.comment "OpacityEnv.pu" OpacityEnv.pu, 
			      IntBasis.pu)))

	type Basis0 = InfixBasis * ElabBasis
	val pu_Basis0 =
	    Pickle.pairGen(InfixBasis.pu, ModuleEnvironments.B.pu)
	fun plusBasis0 ((ib,eb),(ib',eb')) = 
	    (InfixBasis.compose(ib,ib'), ModuleEnvironments.B.plus(eb,eb'))
	fun initialBasis0() = 
	    (InfixBasis.emptyB, ModuleEnvironments.B.initial())
	fun matchBasis0 ((infB,eB), (infB0,eB0)) =
	    let val _ = ModuleEnvironments.B.match(eB,eB0)
	    in (infB,eB)
	    end
	fun eqBasis0((infB1,eB1), (infB2,eB2)) =
	    db_f "InfixBasis" (InfixBasis.eq(infB1,infB2)) andalso 
	    db_f "B_l" (ModuleEnvironments.B.enrich(eB1,eB2)) andalso db_f "B_r" (ModuleEnvironments.B.enrich(eB2,eB1))

	type Basis1 = opaq_env * IntBasis
	val pu_Basis1 =
	    Pickle.pairGen(OpacityEnv.pu, IntBasis.pu)
	fun plusBasis1((oe,ib),(oe',ib')) = 
	    (OpacityEnv.plus(oe,oe'),
	     IntBasis.plus(ib,ib'))
	fun initialBasis1() = (OpacityEnv.initial, 
			       IntBasis.initial())
	fun matchBasis1 ((oe,iB), (oe0,iB0)) =
	    let val _ = OpacityEnv.match(oe,oe0)
		val iB = IntBasis.match(iB,iB0)
	    in (oe,iB)
	    end
	fun eqBasis1((oe1,iB1), (oe2,iB2)) =
	    db_f "OpacityEnv" (OpacityEnv.eq(oe1,oe2)) andalso 
	    db_f "IB_l" (IntBasis.enrich(iB1,iB2)) andalso db_f "IB_r" (IntBasis.enrich(iB2,iB1))

      end

    type name = Name.name
    
  end
