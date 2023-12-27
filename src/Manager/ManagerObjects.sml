(* COMPILER_ENV is the lambda env mapping structure and value
 * identifiers to lambda env's and lvars *)

(* COMPILE_BASIS is the combined basis of all environments in
 * the backend *)

functor ManagerObjects(
                         structure Execution : EXECUTION
                         val program_name : unit -> string
                       ) : MANAGER_OBJECTS =
  struct
    local structure MO =
             ManagerObjects0(structure Execution = Execution)
    in open MO
    end
    structure PP = PrettyPrint
    structure TopdecGrammar = PostElabTopdecGrammar
    structure CompileBasis = Execution.CompileBasis
    structure Labels = AddressLabels

    structure Environment = Environment(struct val program_name = program_name end)

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

    (*
     * Modification times of files
     *)

    type filename = string
    fun funid_from_filename (filename: filename) =    (* contains .sml - hence it cannot *)
      FunId.mk_FunId filename                         (* be declared by the user. *)
    fun funid_to_filename (funid: funid) : filename =
      FunId.pr_FunId funid
    fun mk_filename x = x
    fun filename_to_string x  = x


    (* ----------------------------------------------------
     * Determine where to put target files
     * ---------------------------------------------------- *)

    type target = Execution.target

    val mlbdir = Execution.mlbdir

    type linkinfo = Execution.linkinfo
    structure SystemTools =
      struct
      	(* --------------------
	 * Deleting a file
	 * -------------------- *)

	fun delete_file f = OS.FileSys.remove f handle _ => ()


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
		       let val {dir,file} = OS.Path.splitDirFile filename
			   val target_filename = OS.Path.base(OS.Path.file absprjid) ^ "-" ^ file
			   val target_filename = mlbdir() ## target_filename
			   val _ = maybe_create_mlbdir {prepath=dir}
		       in dir ## target_filename
		       end
		   val f0 = Execution.emit {target=target,filename=target_filename}
		   val f = OS.Path.file f0
	    in (mlbdir() ## f) (* strip the initial dir in the recorded code file *)
               handle OS.Path.Path => die "emit: Path.concat"
	    end

	(* -------------------------------------------------------------
	 * Link time dead code elimination; we eliminate all unnecessary
	 * object files from the link sequence before we do the actual
	 * linking.
	 * ------------------------------------------------------------- *)

	structure labelTable : sig type table
				   val mk : unit -> table
				   val look : table * Execution.lab -> bool
				   val insert : table * Execution.lab -> unit
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
	      let val s = Execution.pr_lab lab
		  val h = hash s
		  val l = Array.sub(table,h)
	      in member s l
	      end
	    fun insert (table,lab) =
	      let val s = Execution.pr_lab lab
		  val h = hash s
		  val l = Array.sub(table,h)
	      in if member s l then ()
		 else Array.update(table,h,s::l)
	      end
	  end

	fun unsafe (tf,li) = Execution.unsafe_linkinfo li
	fun exports (tf,li) = Execution.exports_of_linkinfo li
	fun imports (tf,li) = Execution.imports_of_linkinfo li
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
			    die ("Label " ^ Execution.pr_lab l ^ " allready exported")
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

	val link_files_with_runtime_system = Execution.link_files_with_runtime_system

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
	  in case Execution.generate_link_code of
                 SOME generate_link_code =>
		 let val target_link = generate_link_code (labs, exports)
		     val linkfile_o = emit(target_link, "base", "link_objects")
		 in link_files_with_runtime_system (linkfile_o :: (target_files @ extobjs)) run;
		    delete_file linkfile_o
		 end
	       | NONE =>
		 link_files_with_runtime_system target_files run
	  end

        fun mk_sharedlib (tfiles_with_linkinfos, libs, name, sofile) : unit =
            let val linkinfos = map (fn (a,b) => b) tfiles_with_linkinfos
	        val ofiles = map #1 tfiles_with_linkinfos
	        val labs = map Execution.code_label_of_linkinfo linkinfos
            in Execution.mk_sharedlib(ofiles,labs,libs,name,sofile)
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
        fun mk_modcode (t,l,f) = NOTEMITTED_MODC(t,l,f)

	fun exist EMPTY_MODC = true
	  | exist (SEQ_MODC(mc1,mc2)) = exist mc1 andalso exist mc2
	  | exist (NOTEMITTED_MODC _) = true
	  | exist (EMITTED_MODC(file,_)) =
	  let val res = OS.FileSys.access (file,[]) handle _ => false
	  in if res then res
	     else (print ("File " ^ file ^ " not present\n"); res)
	  end

	fun emit (absprjid: absprjid, modc) =
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

	fun emitted_files (mc,acc) =
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

	fun dirMod' d m =
	    dirMod0 (fn fp => d ## fp) m

	fun absDirMod absd m =
	    dirMod0 (fn fp => absd ## OS.Path.file fp) m

        fun subMod d m =
            let val d = OS.Path.mkCanonical d
                val d = if d = "." then "" else d
            in dirMod0 (fn fp => let val fp = OS.Path.mkCanonical fp
                                 in if OS.Path.isAbsolute fp then fp
                                    else let val fp = if String.isPrefix d fp then
                                                        String.extract(fp,String.size d,NONE)
                                                      else die ("subMod: '" ^ d ^
                                                                "' is not a prefix of '" ^
                                                                fp ^ "'")
                                         in if String.isPrefix "/" fp then
                                              String.extract(fp,1,NONE)
                                            else fp
                                         end
                                 end) m
            end

        fun mk_sharedlib (modc: modcode, libs, name, sofile) : unit =
            (* something like gcc -o sofile -shared (eval.o::files) -init eval_step -llib1 ... -llibn
               where (1) files are the object files in modc and (2) eval.o contains a function that
               calls evaluate code in each modc object file.
             *)
	    let fun get (EMPTY_MODC, acc) = acc
		  | get (SEQ_MODC(modc1,modc2), acc) = get(modc1,get(modc2,acc))
		  | get (EMITTED_MODC p, acc) = p::acc
		  | get (NOTEMITTED_MODC(target,li,filename), acc) = die "mk_sharedlib"
	    in SystemTools.mk_sharedlib(get(modc,[]), libs, name, sofile)
	    end
      end

    val mk_repl_runtime =
        case Execution.generate_repl_init_code of
            SOME f =>
            SOME (fn () =>
                     let val exe = mlbdir() ## "runtime.exe"
                     in if OS.FileSys.access (exe, [OS.FileSys.A_EXEC])
                        then ( pr_debug_linking ("[reusing runtime system " ^ exe ^ "]\n")
                             ; exe )
                        else let val t = f()
                                 val link_file_base = mlbdir() ## "repl-link"
                                 val () = SystemTools.maybe_create_mlbdir {prepath=""}
                                 val link_file_object = Execution.emit{target=t,filename=link_file_base}
                                 val exe = Execution.create_repl_runtime [link_file_object] (mlbdir())
                             in pr_debug_linking ("[created runtime system " ^ exe ^ "]\n")
                              ; exe
                             end
                     end)
          | NONE => NONE

    val toJSString = Execution.toJSString
    val export_basis_js = Execution.export_basis_js

  end
