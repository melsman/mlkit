(* COMPILER_ENV is the lambda env mapping structure and value 
 * identifiers to lambda env's and lvars *)

(* COMPILE_BASIS is the combined basis of all environments in 
 * the backend *) 

functor ManagerObjects(structure ModuleEnvironments : MODULE_ENVIRONMENTS
		       structure TopdecGrammar : TOPDEC_GRAMMAR   (*needed for type strexp*)
			 sharing type TopdecGrammar.funid = ModuleEnvironments.funid
			     and type TopdecGrammar.id = ModuleEnvironments.id
			     and type TopdecGrammar.longtycon = ModuleEnvironments.longtycon
			     and type TopdecGrammar.longstrid = ModuleEnvironments.longstrid
		       structure OpacityElim : OPACITY_ELIM
			 sharing OpacityElim.TyName = ModuleEnvironments.TyName
			     and type OpacityElim.OpacityEnv.realisation = ModuleEnvironments.realisation
			     and type OpacityElim.topdec = TopdecGrammar.topdec
		       structure CompilerEnv : COMPILER_ENV
			 sharing type CompilerEnv.id = ModuleEnvironments.id
			     and type CompilerEnv.longid = TopdecGrammar.DecGrammar.Ident.longid
			     and type CompilerEnv.strid = ModuleEnvironments.strid
			     and type CompilerEnv.longstrid = ModuleEnvironments.longstrid
			     and type CompilerEnv.tycon = ModuleEnvironments.tycon
			     and type CompilerEnv.longtycon = ModuleEnvironments.longtycon
		       structure CompileBasis : COMPILE_BASIS
			 sharing type CompileBasis.lvar = CompilerEnv.lvar
			     and type CompileBasis.TyName = ModuleEnvironments.TyName
			               = CompilerEnv.TyName
			     and type CompileBasis.con = CompilerEnv.con
			     and type CompileBasis.excon = CompilerEnv.excon
		       structure Compile : COMPILE
		       structure InfixBasis: INFIX_BASIS
		       structure ElabRep : ELAB_REPOSITORY
			 sharing type ElabRep.funid = TopdecGrammar.funid 
			     and type ElabRep.InfixBasis = InfixBasis.Basis
			     and type ElabRep.ElabBasis = ModuleEnvironments.Basis
			     and type ElabRep.opaq_env = OpacityElim.opaq_env
			     and type ElabRep.longstrid = ModuleEnvironments.longstrid
			     and ElabRep.TyName = ModuleEnvironments.TyName
		       structure FinMap : FINMAP
		       structure PP : PRETTYPRINT
			 sharing type PP.StringTree = CompilerEnv.StringTree 
			   = CompileBasis.StringTree = ModuleEnvironments.StringTree
			   = FinMap.StringTree = InfixBasis.StringTree = OpacityElim.OpacityEnv.StringTree
		       structure Name : NAME
			 sharing type Name.name = ModuleEnvironments.TyName.name = ElabRep.name
		       structure Flags : FLAGS
		       structure Crash : CRASH) : MANAGER_OBJECTS =
  struct

    fun die s = Crash.impossible("ManagerObjects." ^ s)
    fun chat s = if !Flags.chat then print (s ^ "\n") else ()

    local
      val debug_linking = ref false
      val _ = Flags.add_flag_to_menu(["Debug Kit", "Manager"], 
				     "debug_linking", "debug_linking", debug_linking)
    in
      fun pr_debug_linking s = if !debug_linking then print s else ()
    end

    structure FunId = TopdecGrammar.FunId
    structure TyName = ModuleEnvironments.TyName
    type StringTree = PP.StringTree
    type filename = string
    type target = Compile.target

    (* ----------------------------------------------------
     * Determine where to put target files; if profiling is
     * enabled then we put target files into the PM/Prof/
     * directory; otherwise, we put target files into the
     * PM/NoProf/ directory.
     * ---------------------------------------------------- *)

    local
      val region_profiling = Flags.lookup_flag_entry "region_profiling"
    in fun pmdir() = if !region_profiling then "PM/Prof/" else "PM/NoProf/"
    end

   (* -----------------------------------------------------------------
    * Execute shell command and return the result code.
    * ----------------------------------------------------------------- *)

    structure Shell =
      struct
	exception Execute of string
	fun execute_command command : unit =
	  let val status = OS.Process.system command
	            handle OS.SysErr(s,_) =>
		      raise Execute ("Exception OS.SysErr \""
				     ^ s ^ "\"\nwhen executing shell command:\n"
			             ^ command)
	  in if status <> OS.Process.success then
	        raise Execute ("Error code " ^ Int.toString status ^
			       " when executing shell command:\n"
			       ^ command)
	     else ()
	  end
      end

    type linkinfo = Compile.linkinfo
    structure SystemTools =
      struct
	val c_compiler = Flags.lookup_string_entry "c_compiler"
	val c_libs = Flags.lookup_string_entry "c_libs"

	(*logging*)
	val log_to_file = Flags.lookup_flag_entry "log_to_file"

	(*targets*)
	val target_file_extension = Flags.lookup_string_entry "target_file_extension"

	(*linking*)
	val region_profiling = Flags.lookup_flag_entry "region_profiling"
	fun path_to_runtime () = ! (Flags.lookup_string_entry
				    (if !region_profiling then "path_to_runtime_prof"
				     else "path_to_runtime"))


	(* -----------------------------
	 * Append functions
	 * ----------------------------- *)
	  
	fun append_ext s = s ^ !target_file_extension
	fun append_o s = s ^ ".o"

	(* --------------------
	 * Deleting a file
	 * -------------------- *)

	fun delete_file f = OS.FileSys.remove f handle _ => ()

	(* -------------------------------
	 * Assemble a file into a .o-file
	 *-------------------------------- *)

	fun assemble (file_s, file_o) =
          (Shell.execute_command (!c_compiler ^ " -c -o " ^ file_o ^ " " ^ file_s);
	   if !(Flags.lookup_flag_entry "delete_target_files")
             then  delete_file file_s 
           else ())

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

	(* -----------------------------------------------
	 * Emit assembler code and assemble it. 
	 * ----------------------------------------------- *)

	fun emit (target, target_filename) =
	  let val target_filename = pmdir() ^ target_filename
	      val target_filename = OS.Path.mkAbsolute(target_filename, OS.FileSys.getDir())
              val target_filename_s = append_ext target_filename
	      val target_filename_o = append_o target_filename
	      val _ = Compile.emit {target=target,filename=target_filename_s}
	      val _ = assemble (target_filename_s, target_filename_o)
	  in target_filename_o
	  end

	(* -------------------------------------------------------------
	 * Link time dead code elimination; we eliminate all unnecessary
	 * object files from the link sequence before we do the actual
	 * linking. 
	 * ------------------------------------------------------------- *)

	structure EATable : sig type table
				val mk : unit -> table
				val look : table * Compile.EA -> bool
				val insert : table * Compile.EA -> unit
			    end =
	  struct
	    type table = (string list) Array.array
	    val table_size = 1009
	    val table_size_word = Word.fromInt table_size
	    fun hash s =
	      let fun loop (0, acc) = acc
		    | loop (i, acc) = loop(i-1, Word.+(Word.*(0w19,acc), Word.fromInt(Char.ord(String.sub(s,i-1)))))
	      in Word.toInt(Word.mod(loop (String.size s, 0w0), table_size_word))
	      end
	    fun mk () = Array.array (table_size, nil)
	    fun member (a:string) l =
	      let fun f [] = false
		    | f (x::xs) = a=x orelse f xs 
	      in f l
	      end
	    fun look (table,ea) =
	      let val s = Compile.pp_EA ea
		  val h = hash s
		  val l = Array.sub(table,h)
	      in member s l
	      end
	    fun insert (table,ea) = 
	      let val s = Compile.pp_EA ea
		  val h = hash s
		  val l = Array.sub(table,h)
	      in if member s l then ()
		 else Array.update(table,h,s::l)
	      end
(*	    fun reset () =
	      let fun loop 0 = ()
		    | loop i = Array.update(table,i-1,nil)
	      in loop table_size
	      end 
*)
	  end

	fun unsafe(tf,li) = Compile.unsafe_linkinfo li
	fun exports(tf,li) = Compile.exports_of_linkinfo li
	fun imports(tf,li) = Compile.imports_of_linkinfo li
	fun dead_code_elim tfiles_with_linkinfos = 
	  let 
	    val _ = pr_debug_linking "[Link time dead code elimination begin...]\n"
	    val table = EATable.mk()
	    fun require eas : unit = List.app (fn ea => EATable.insert(table,ea)) eas
	    fun required eas : bool = foldl (fn (ea,acc) => acc orelse EATable.look(table,ea)) false eas
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

	(* -------------------------------------------------------------
	 * link_files_with_runtime_system files run : Link a list `files' of
	 * partially linked files (.o files) to the runtime system
	 * (also partially linked) and produce an executable called `run'.
	 * ------------------------------------------------------------- *)

	fun link_files_with_runtime_system files run =
          let val files = map (fn s => s ^ " ") files
	  in
	    (Shell.execute_command
	     (!c_compiler ^ " -o " ^ run ^ " " ^ concat files
	      ^ path_to_runtime () ^ " " ^ !c_libs)
              (*see comment at `assemble' above*);
	     TextIO.output (TextIO.stdOut, "[wrote executable file:\t" ^ run ^ "]\n"))
	  end handle Shell.Execute s => die ("link_files_with_runtime_system:\n" ^ s)

	fun member f [] = false
	  | member f ( s :: ss ) = f = s orelse member f ss

	fun elim_dupl ( [] , acc )  = acc
	  | elim_dupl ( f :: fs , acc ) = elim_dupl ( fs, if member f acc then acc else f :: acc )

	(* --------------------------------------------------------------
	 * link (target_files,linkinfos): Produce a link file "link.s". 
	 * Then link the entire project and produce an executable "run".
	 * -------------------------------------------------------------- *)

	fun link (tfiles_with_linkinfos, extobjs, run) : unit =
	  let val tfiles_with_linkinfos = dead_code_elim tfiles_with_linkinfos
	      val linkinfos = map #2 tfiles_with_linkinfos
	      val target_files = map #1 tfiles_with_linkinfos
	      val eas = map Compile.code_label_of_linkinfo linkinfos
	      val extobjs = elim_dupl (extobjs,[])
	      val target_link = Compile.generate_link_code eas
	      val linkfile = pmdir() ^ "link_objects"
	      val linkfile_s = append_ext linkfile
	      val linkfile_o = append_o linkfile
	      val _ = Compile.emit {target=target_link, filename=linkfile_s}
	      val _ = assemble (linkfile_s, linkfile_o)
	  in link_files_with_runtime_system (linkfile_o :: (target_files @ extobjs)) run;
	    if !(Flags.lookup_flag_entry "delete_target_files") then delete_file linkfile_o
	    else ()
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
        val mk_modcode = NOTEMITTED_MODC

	fun exist EMPTY_MODC = true
	  | exist (SEQ_MODC(mc1,mc2)) = exist mc1 andalso exist mc2
	  | exist (NOTEMITTED_MODC _) = true
	  | exist (EMITTED_MODC(file,_)) = OS.FileSys.access (file,[]) handle _ => false

	fun emit(prjid, modc) =
	  let fun em EMPTY_MODC = EMPTY_MODC
		| em (SEQ_MODC(modc1,modc2)) = SEQ_MODC(em modc1, em modc2)
		| em (EMITTED_MODC(fp,li)) = EMITTED_MODC(fp,li)
		| em (NOTEMITTED_MODC(target,linkinfo,filename)) = 
	              EMITTED_MODC(SystemTools.emit(target, OS.Path.base prjid ^ "-" ^ filename),linkinfo)
                           (*puts ".o" on filename*)
	  in em modc
	  end

	fun mk_exe (prjid, modc, extobjs, run) =
	  let fun get (EMPTY_MODC, acc) = acc
		| get (SEQ_MODC(modc1,modc2), acc) = get(modc1,get(modc2,acc))
		| get (EMITTED_MODC p, acc) = p::acc
		| get (NOTEMITTED_MODC(target,li,filename), acc) =
	             (SystemTools.emit(target, OS.Path.base prjid ^ "-" ^ filename),li)::acc
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
    type TopCompileBasis = CompileBasis.TopCompileBasis

    type strexp = TopdecGrammar.strexp
    type strid = ModuleEnvironments.strid
    type prjid = ModuleEnvironments.prjid

    datatype IntFunEnv = IFE of (funid, prjid * funstamp * strid * ElabEnv * (unit -> strexp) * IntBasis) FinMap.map
         and IntBasis = IB of IntFunEnv * CEnv * CompileBasis
	   
    datatype TopIntBasis = TIB of IntFunEnv * CEnv * TopCompileBasis

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
	     | NONE => die "IntFunEnv.lookup"
	fun restrict (IFE ife, funids) = IFE
	  (foldl (fn (funid, acc) =>
		  case FinMap.lookup ife funid
		    of SOME e => FinMap.add(funid,e,acc)
		     | NONE => die "IntFunEnv.restrict") FinMap.empty funids)
	fun enrich(IFE ife0, IFE ife) : bool = (* using funstamps; enrichment for free variables is checked *)
	  FinMap.Fold(fn ((funid, obj), b) => b andalso         (* when the functor is being declared!! *)
		      case FinMap.lookup ife0 funid
			of SOME obj0 => FunStamp.eq(#2 obj,#2 obj0) andalso #1 obj = #1 obj0
			 | NONE => false) true ife
	fun layout (IFE ife) = FinMap.layoutMap{start="IntFunEnv = [", eq="->",sep=", ", finish="]"}
	  (PP.LEAF o FunId.pr_FunId) (PP.LEAF o FunStamp.pr o #2) ife
      end

    type longid = TopdecGrammar.DecGrammar.Ident.longid
    type longstrid = TopdecGrammar.StrId.longstrid
    type longtycon = TopdecGrammar.DecGrammar.TyCon.longtycon
    structure IntBasis =
      struct
	val mk = IB
	fun un (IB ib) = ib
	val empty = IB (IntFunEnv.empty, CompilerEnv.emptyCEnv, CompileBasis.empty)
	fun plus (IB(ife1,ce1,cb1), IB(ife2,ce2,cb2)) =
	  IB(IntFunEnv.plus(ife1,ife2), CompilerEnv.plus(ce1,ce2), CompileBasis.plus(cb1,cb2))
	fun plus' (TIB(ife1,ce1,tcb1), TIB(ife2,ce2,tcb2)) =
	  TIB(IntFunEnv.plus(ife1,ife2), CompilerEnv.plus(ce1,ce2), CompileBasis.plus'(tcb1,tcb2))
	local
	  fun restr (cb_restr, (ife,ce,cb), {funids, longstrids, longvids, longtycons}) =
	    let val ife' = IntFunEnv.restrict(ife,funids)
	        val ce' = CompilerEnv.restrictCEnv(ce,{longstrids=longstrids,longvids=longvids,longtycons=longtycons})
		(*val _ = if !Flags.chat then (print("\n RESTRICTED CE:\n");PP.outputTree(print,CompilerEnv.layoutCEnv ce',100))
			else ()*)
		val lvars = CompilerEnv.lvarsOfCEnv ce'
		val lvars_with_prims = lvars @ (CompilerEnv.primlvarsOfCEnv ce')
		fun tynames_ife(IFE ife, tns) = 
		  let fun tynames_obj ((_,_,_,_,_,obj),tns) = 
		        let val IB(_,ce,_) = obj
			in CompilerEnv.tynamesOfCEnv ce @ tns
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
		val cons = CompilerEnv.consOfCEnv ce'
		val excons = CompilerEnv.exconsOfCEnv ce'
		val cb' = cb_restr(cb,(lvars,lvars_with_prims,tynames,cons,excons))
	    in (ife',ce',cb')
	    end
	in fun restrict (IB ib, res) = IB(restr(CompileBasis.restrict, ib, res))
	   fun restrict' (TIB tib, res) = IB(restr(CompileBasis.restrict', tib, res))
	end 

	fun match(IB(ife1,ce1,cb1),IB(ife2,ce2,cb2)) =
	  let val _ = CompilerEnv.match(ce1,ce2)
	      val cb1' = CompileBasis.match(cb1,cb2)
	  in IB(ife1,ce1,cb1')
	  end

	local 
	  fun IntFunEnv_enrich a = IntFunEnv.enrich a
	  fun CompilerEnv_enrichCEnv a = CompilerEnv.enrichCEnv a
	  fun CompileBasis_enrich a = CompileBasis.enrich a
	  fun CompileBasis_enrich' a = CompileBasis.enrich' a
	  fun enrich0(enr,(ife0,ce0,cb0),IB(ife,ce,cb)) =
	    IntFunEnv_enrich(ife0,ife) andalso CompilerEnv_enrichCEnv(ce0,ce) 
	    andalso enr(cb0,cb)
	in
	  fun enrich(IB IB1, IB2) = enrich0(CompileBasis_enrich, IB1, IB2)
	  fun enrich'(TIB IB1, IB2) = enrich0(CompileBasis_enrich', IB1, IB2)
	end

	local
	  fun agree1(rest, longstrid, (_,ce1,cb1), IB(_,ce2,cb2)) =
	    let val ce1 = CompilerEnv.lookup_longstrid ce1 longstrid
	        val ce2 = CompilerEnv.lookup_longstrid ce2 longstrid
	    in
	      CompilerEnv.enrichCEnv(ce1,ce2) andalso CompilerEnv.enrichCEnv(ce2,ce1) andalso
	      let 
		fun restr rest ce cb =
		  let val lvars = CompilerEnv.lvarsOfCEnv ce
		      val lvars_with_prims = lvars @ (CompilerEnv.primlvarsOfCEnv ce)
		      val tynames = CompilerEnv.tynamesOfCEnv ce
		      val cons = CompilerEnv.consOfCEnv ce
		      val excons = CompilerEnv.exconsOfCEnv ce
		  in rest(cb,(lvars,lvars_with_prims,tynames,cons,excons))
		  end
		val cb1 = restr rest ce1 cb1
		val cb2 = restr CompileBasis.restrict ce2 cb2
	      in CompileBasis.eq(cb1,cb2)
	      end
	    end
	  fun agree2 (rest, [], _,_) = true
	    | agree2 (rest, longstrid::longstrids, B1, B2) = 
	    agree1(rest, longstrid, B1, B2) andalso agree2(rest, longstrids, B1, B2)
	in
	  fun agree (l, IB B1, B2) = agree2 (CompileBasis.restrict, l, B1, B2) 
	  fun agree' (l, TIB B1, B2) = agree2 (CompileBasis.restrict', l, B1, B2) 
	end

	fun layout(IB(ife,ce,cb)) =
	  PP.NODE{start="IntBasis = [", finish="]", indent=1, childsep=PP.RIGHT ", ",
		  children=[IntFunEnv.layout ife,
			    CompilerEnv.layoutCEnv ce,
			    CompileBasis.layout_CompileBasis cb]}
	  
	  (* operations used in Manager, only. *)
	val initial = TIB (IntFunEnv.initial, CompilerEnv.initialCEnv, CompileBasis.initial)
	fun topify (IB(ife,ce,cb)) = TIB(ife,ce,CompileBasis.topify cb)
      end

    type ElabBasis = ModuleEnvironments.Basis 
    type InfixBasis = InfixBasis.Basis
    type sigid = ModuleEnvironments.sigid
    type opaq_env = OpacityElim.opaq_env     
    datatype Basis = BASIS of InfixBasis * ElabBasis * opaq_env * IntBasis
    datatype TopBasis = TOPBASIS of InfixBasis * ElabBasis * opaq_env * TopIntBasis
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
	  fun IntBasis_enrich' a = IntBasis.enrich' a
	in
	  fun enrich (TOPBASIS (infB1,elabB1,rea1,tintB1), (BASIS (infB2,elabB2,rea2,intB2), dom_rea)) = 
	    debug("InfixBasis", InfixBasis_eq(infB1,infB2)) andalso 
	    debug("ElabBasis", ModuleEnvironments_B_enrich (elabB1,elabB2)) andalso
	    debug("OpacityEnv", OpacityElim_enrich (rea1,(rea2,dom_rea))) andalso
	    debug("IntBasis", IntBasis_enrich'(tintB1,intB2))
	end

	fun agree(longstrids, TOPBASIS(_,elabB1,rea1,tintB1), (BASIS(_,elabB2,rea2,intB2), dom_rea)) =
	  ModuleEnvironments.B.agree(longstrids,elabB1,elabB2) andalso IntBasis.agree'(longstrids,tintB1,intB2)
	  
	fun layout (BASIS(infB,elabB,rea,intB)) : StringTree =
	  PP.NODE{start="BASIS(", finish = ")",indent=1,childsep=PP.RIGHT ", ",
		  children=[InfixBasis.layoutBasis infB, ModuleEnvironments.B.layout elabB,
			    OpacityElim.OpacityEnv.layout rea, IntBasis.layout intB]}

	val initial = TOPBASIS (InfixBasis.emptyB, ModuleEnvironments.B.initial, OpacityElim.OpacityEnv.initial, IntBasis.initial)
	fun plus' (TOPBASIS (infb,elabb,rea,tintb), TOPBASIS (infb',elabb',rea',tintb')) =
	  TOPBASIS (InfixBasis.compose(infb,infb'), ModuleEnvironments.B.plus (elabb, elabb'),
		    OpacityElim.OpacityEnv.plus(rea,rea'), IntBasis.plus'(tintb, tintb'))

	fun topify (BASIS(infB,elabB,rea,intB)) = TOPBASIS(infB,elabB,rea,IntBasis.topify intB)

	fun un' (TOPBASIS a) = a
      end


    type name = Name.name
    structure Repository =
      struct
	type intRep = ((prjid * funid) * bool, (funstamp * ElabEnv * IntBasis * longstrid list * 
						name list * modcode * IntBasis) list) FinMap.map ref
	  (* the bool is true if profiling is enabled *)
	val region_profiling : bool ref = Flags.lookup_flag_entry "region_profiling"

	val intRep : intRep = ref FinMap.empty
	fun clear() = (ElabRep.clear();
		       List.app (List.app (ModCode.delete_files o #6)) (FinMap.range (!intRep));  
		       intRep := FinMap.empty)
	fun delete_rep rep prjid_and_funid = case FinMap.remove ((prjid_and_funid, !region_profiling), !rep)
				     of Edlib.General.OK res => rep := res
				      | _ => ()
	fun delete_entries prjid_and_funid = (ElabRep.delete_entries prjid_and_funid; 
					      delete_rep intRep prjid_and_funid)
	fun lookup_rep rep exportnames_from_entry prjid_and_funid =
	  let val all_gen = foldl (fn (n, b) => b andalso Name.is_gen n) true
	      fun find ([], n) = NONE
		| find (entry::entries, n) = 
		if (all_gen o exportnames_from_entry) entry then SOME(n,entry)
		else find(entries,n+1)
	  in case FinMap.lookup (!rep) (prjid_and_funid, !region_profiling)
	       of SOME entries => find(entries, 0)
		| NONE => NONE
	  end

	fun add_rep rep (prjid_and_funid,entry) : unit =
	  rep := let val r = !rep 
		     val i = (prjid_and_funid, !region_profiling)
		 in case FinMap.lookup r i
		      of SOME res => FinMap.add(i,res @ [entry],r)
		       | NONE => FinMap.add(i,[entry],r)
		 end

	fun owr_rep rep (prjid_and_funid,n,entry) : unit =
	  rep := let val r = !rep
		     val i = (prjid_and_funid, !region_profiling)
	             fun owr(0,entry::res,entry') = entry'::res
		       | owr(n,entry::res,entry') = entry:: owr(n-1,res,entry')
		       | owr _ = die "owr_rep.owr"
		 in case FinMap.lookup r i
		      of SOME res => FinMap.add(i,owr(n,res,entry),r)
		       | NONE => die "owr_rep.NONE"
		 end
	val lookup_int = lookup_rep intRep #5

	fun add_int (prjid_and_funid,entry) = 
	  if ModCode.all_emitted (#6 entry) then  (* just make sure... *)
	    add_rep intRep (prjid_and_funid, entry)
	  else die "add_int"

	fun owr_int (prjid_and_funid,n,entry) =
	  if ModCode.all_emitted (#6 entry) then  (* just make sure... *)
	    owr_rep intRep (prjid_and_funid,n,entry)
	  else die "owr_int"

	fun recover_intrep() =
	  List.app 
	  (List.app (fn entry => List.app Name.mark_gen (#5 entry)))
	  (FinMap.range (!intRep))

	fun emitted_files() =
	  let fun files_entries ([],acc) = acc
		| files_entries ((_,_,_,_,_,mc,_)::entries,acc) = 
		    files_entries(entries,ModCode.emitted_files(mc,acc))
	  in FinMap.fold files_entries [] (!intRep)
	  end
	val lookup_elab = ElabRep.lookup_elab
	val add_elab = ElabRep.add_elab
	val owr_elab = ElabRep.owr_elab
	fun recover() = (ElabRep.recover(); recover_intrep())
      end
    
  end
