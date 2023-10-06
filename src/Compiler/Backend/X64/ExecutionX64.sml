
structure ExecutionX64: EXECUTION =
  struct
    structure TopdecGrammar = PostElabTopdecGrammar
    structure Labels = AddressLabels
    structure PP = PrettyPrint

    structure BackendInfo =
      BackendInfo(val down_growing_stack : bool = true)          (* true for x64 code generation *)

    structure NativeCompile = NativeCompile(structure BackendInfo = BackendInfo
                                            structure RegisterInfo = InstsX64.RI)

    structure CompileBasis = CompileBasis(structure ClosExp = NativeCompile.ClosExp)

    structure JumpTables = JumpTables(BackendInfo)

    structure CodeGen = CodeGenX64(structure BackendInfo = BackendInfo
                                   structure JumpTables = JumpTables
                                   structure CallConv = NativeCompile.CallConv
                                   structure LineStmt = NativeCompile.LineStmt
                                   structure SubstAndSimplify = NativeCompile.SubstAndSimplify)

    fun die s = Crash.impossible("ExecutionX64." ^ s)

    fun onmac_p () = InstsX64.sysname() = "Darwin"

    val be_rigid = false

    local
        fun convertList option s =
            let val l = String.tokens(fn c => c = #",")s
            in map (fn s => option ^ s) l
            end
    in
        fun libConvertList s = concat(convertList " -l" s)
        fun libdirsConvertList s = concat(convertList " -L" s)
    end

    local val default = "m,c,dl"
    in
        val _ = Flags.add_string_entry
            {long="libs", short=NONE, item=ref default,
             menu=["Control", "foreign libraries (archives)"],
             desc="For accessing a foreign function residing in\n\
              \an archive named libNAME.a from Standard ML code\n\
              \(using prim), you need to add 'NAME' to this\n\
              \comma-separated list. Notice that an object file\n\
              \(with extension '.o') is an archive if it is\n\
              \renamed to have extension '.a'. You may need to\n\
              \use the -libdirs option for specifying\n\
              \directories for which ld should look for library\n\
              \archives. The libraries are passed to 'ld' using\n\
              \the -l option."}
    end

    val _ = Flags.add_string_entry
      {long="libdirs", short=NONE, item=ref "",
       menu=["Control", "library directories (paths to archives)"],
       desc="This option controls where ld looks for\n\
        \archives. The format is a comma-separated list\n\
        \of directories; see the -libs entry. The default\n\
        \is the empty list; thus 'ld' will look for\n\
        \libraries in only the system specific default\n\
        \directores. The directories are passed to 'ld'\n\
        \using the -L option."}

    val _ = Flags.add_string_entry
        let val macgcc = "gcc -Wl,-stack_size,0x10000000,-stack_addr,0xc0000000"
            val gcc = if onmac_p() then macgcc
                      else "gcc"
        in
            {long="c_compiler", short=SOME "cc", item=ref gcc,
             menu=["Control", "C compiler (used for linking)"],
             desc="This option specifies which C compiler is\n\
                  \used for linking. When linking with c++\n\
                  \libraries, 'g++' is the linker you want.\n\
                  \On Linux the default is 'gcc -no-pie',\n\
                  \whereas on macOS, the default is\n\
                  \'" ^ macgcc ^ "'."}
        end

    val _ = Flags.add_string_entry
        let val mac_as = "as -q" (* "gcc -c -no-integrated-as" *)
            val linux_as = "as --64"
            val ass = if onmac_p() then mac_as else linux_as
        in
            {long="assembler", short=SOME "as", item=ref ass,
             menu=["Control", "Assembler command"],
             desc="This option specifies the assembler used.\n\
                  \On Linux the default is '" ^ linux_as ^ "'. On macOS,\n\
                  \the default is '" ^ mac_as ^ "'."}
        end

    val strip_p = ref false
    val _ = Flags.add_bool_entry
       {long="strip", short=NONE, neg=false, item=strip_p,
        menu=["Control", "strip executable"],
        desc="If enabled, the Kit strips the generated executable."}

    val _ = Flags.add_bool_entry
       {long="delete_target_files", short=NONE, neg=true, item=ref true,
        menu=["Debug", "delete target files"],
        desc="Delete assembler files produced by the compiler. If you\n\
         \disable this flag, you can inspect the assembler code\n\
         \produced by the compiler."}

    local
      val desc =
          "When enabled, the compiler passes the option --gstabs\n\
          \to `as' (The GNU Assembler) and preserves the generated\n\
          \assembler files (.s files). Passing the --gstabs\n\
          \option to `as' makes it possible to step through\n\
          \the generated program using gdb (The GNU Debugger)."
      val desc_darwin =
          "When enabled, the compiler passes the option --g\n\
          \to `as' (The GNU Assembler) and preserves the generated\n\
          \assembler files (.s files). Passing the --g\n\
          \option to `as' makes it possible to step through\n\
          \the generated program using gdb (The GNU Debugger)."
    in
      val _ = Flags.add_bool_entry
        {long="gdb_support", short=SOME "g", neg=false,
         menu=["Debug","gdb support"], item=ref false,
         desc=if onmac_p() then desc_darwin else desc}
    end

    val dangle_stat_p = ref false
    val _ = Flags.add_bool_entry
        {long="dangling_pointers_statistics", short=NONE, neg=false,
         menu=["Debug","dangling pointers statistics"], item=dangle_stat_p,
         desc="When enabled, the compiler prints statistics about\n\
          \the number of times strengthening of the region typing\n\
          \rules (to avoid dangling pointers during evaluation)\n\
          \effects the target program. This flag is useful only\n\
          \when the flag -gc or -no_dangle is enabled."}

    fun report_dangle_stat () =
         if !dangle_stat_p then
           let val n = !Flags.Statistics.no_dangling_pointers_changes
               val total = !Flags.Statistics.no_dangling_pointers_changes_total
           in
               print ("Dangling pointers statistics: \n\
                      \ * Number of changes due to strengthening of typing \n\
                      \   rules to avoid dangling pointers: " ^ Int.toString n ^
                      "\n * Total number of changes: " ^ Int.toString total ^ "\n")
           end
         else ()

    val _ = Flags.add_bool_entry
        {long="parallelism", short=SOME "par", neg=false,
         menu=["Control","parallelism"], item=ref false,
         desc="When enabled, the runtime system supports\n\
          \parallel threads."}

    val parallelism_p = Flags.is_on0 "parallelism"

    val _ = Flags.add_bool_entry
        {long="parallelism_alloc_unprotected", short=SOME "par0", neg=false,
         menu=["Control","parallelism allocation unprotected"], item=ref false,
         desc="When enabled, allocation into a region is not\n\
          \guaranteed to be atomic."}

    val par_alloc_unprotected_p = Flags.is_on0 "parallelism_alloc_unprotected"

    val _ = Flags.add_bool_entry
        {long="argobots", short=SOME "argo", neg=false,
         menu=["Control","use the Argobots lightweight thread library"], item=ref false,
         desc="When enabled, executables link with the Argobots\n\
              \lightweight thread library."}

    val argobots_p = Flags.is_on0 "argobots"

    local val default = ""
    in
        val _ = Flags.add_string_entry
            {long="mlb-subdir", short=NONE, item=ref default,
             menu=["Control", "Use MLB subdir-postfix"],
             desc="For ensuring that the smart recompilation scheme\n\
              \is not reusing target-code compiled with different\n\
              \settings, a string provided with the mlb-subdir\n\
              \option can ensure the use of consistently generated\n\
              \code. This option is Useful, in particular, when\n\
              \performing benchmarking."}
    end

    val backend_name = "X64"

    type CompileBasis = CompileBasis.CompileBasis
    type CEnv = CompilerEnv.CEnv
    type Env = CompilerEnv.ElabEnv
    type strdec = TopdecGrammar.strdec
    type strexp = TopdecGrammar.strexp
    type funid = TopdecGrammar.funid
    type strid = TopdecGrammar.strid
    type target = CodeGen.AsmPrg
    type lab = NativeCompile.label

    val pr_lab = Labels.pr_label

    type linkinfo = {code_label:lab, imports: lab list * lab list,
                     exports : lab list * lab list, unsafe:bool}
    fun code_label_of_linkinfo (li:linkinfo) = #code_label li
    fun exports_of_linkinfo (li:linkinfo) = #exports li
    fun imports_of_linkinfo (li:linkinfo) = #imports li
    fun unsafe_linkinfo (li:linkinfo) = #unsafe li
    fun mk_linkinfo a : linkinfo = a

    val debug_linking = Flags.is_on0 "debug_linking"

    (* Hook to be run before any compilation *)
    val preHook : unit -> unit = Compile.preHook

    (* Hook to be run after all compilations (for one compilation unit) *)
    val postHook : {unitname:string} -> unit = Compile.postHook

    datatype res = CodeRes of CEnv * CompileBasis * target * linkinfo
                 | CEnvOnlyRes of CEnv

    fun compile fe (ce, CB, strdecs, vcg_file) =
      let val (cb,closenv) = CompileBasis.de_CompileBasis CB
      in
        case Compile.compile fe (ce, cb, strdecs)
          of Compile.CEnvOnlyRes ce => CEnvOnlyRes ce
           | Compile.CodeRes(ce,cb,target,safe) =>
            let
              val (closenv, target_new) = NativeCompile.compile(closenv,target,safe,vcg_file)
              val {main_lab, code, imports, exports, safe} = target_new
              val asm_prg = Timing.timing "CG" CodeGen.CG target_new
              val linkinfo = mk_linkinfo {code_label=main_lab,
                                          imports=imports, (* (MLFunLab, DatLab) *)
                                          exports=exports, (* (MLFunLab, DatLab) *)
                                          unsafe=not(safe)}
              val CB = CompileBasis.mk_CompileBasis(cb,closenv)
            in
              CodeRes(ce,CB,asm_prg,linkinfo)
            end
      end
    val generate_link_code = SOME (fn (labs,exports) => CodeGen.generate_link_code (labs,exports))

    fun delete_file f = OS.FileSys.remove f handle _ => ()

    fun execute_command cmd : unit =
        let val () = if debug_linking() then print ("[Executing: " ^ cmd ^ "]\n")
                     else ()
        in (OS.Process.system cmd; ())
           handle (X as OS.SysErr(s,_)) =>
                  ( print ("\nCommand " ^ cmd ^ "\nfailed (" ^ s ^ ")\n")
                  ; raise X)
        end

    val gdb_support = Flags.is_on0 "gdb_support"
    val delete_target_files = Flags.is_on0 "delete_target_files"
    val libs = Flags.lookup_string_entry "libs"

    fun gas0 () =
        !(Flags.lookup_string_entry "assembler")

    fun gas () = if gdb_support() then
                   if onmac_p() then gas0() ^ " -g"
                   else gas0() ^ " --gstabs"
                 else gas0()

    fun assemble (file_s, file_o) =
      (execute_command (gas() ^ " -o " ^ file_o ^ " " ^ file_s);
       if delete_target_files() andalso not(gdb_support()) then delete_file file_s
       else ())

    fun emit {target, filename:string} : string =
      let val filename_o = filename ^ ".o"
          val filename_s = filename ^ ".s"
      in CodeGen.emit (target, filename_s);
        assemble(filename_s, filename_o);
        filename_o
      end

    fun strip run =
      if !strip_p then (execute_command ("strip " ^ run)
                        handle _ => ())
      else ()

    fun link_files_with_runtime_system0 path_to_runtime files run =
      let val files = map (fn s => s ^ " ") files
          val libdirs =
              case !(Flags.lookup_string_entry "libdirs") of
                  "" => ""
                | libdirs => " " ^ libdirsConvertList libdirs

          val pthread = if parallelism_p() andalso not(onmac_p())
                        then " -pthread"
                        else ""
          val cc = !(Flags.lookup_string_entry "c_compiler")
          val shell_cmd = cc ^ " -o " ^ run ^ " " ^
            concat files ^ path_to_runtime() ^ libdirs ^ libConvertList(!libs) ^ pthread
      in
        execute_command shell_cmd;
        strip run;
        print("[wrote executable file:\t" ^ run ^ "]\n");
        report_dangle_stat()
      end

    val op ## = OS.Path.concat infix ##

    local
          val region_profiling = Flags.lookup_flag_entry "region_profiling"
          val tag_values = Flags.is_on0 "tag_values"
          val tag_pairs_p = Flags.is_on0 "tag_pairs"
          val gc_p = Flags.is_on0 "garbage_collection"
          val gengc_p = Flags.is_on0 "generational_garbage_collection"

          fun path_to_runtime () =
              let
                fun file () =
                    if parallelism_p() then
                      (if tag_values() then
                         die "parallelism enabled - turn off value tagging"
                       else if gc_p() then
                         die "parallelism enabled - turn off gc"
                       else if !region_profiling then
                         die "parallelism enabled - turn off prof"
                       else if tag_pairs_p() then
                         die "parallelism enabled - turn off pair tagging"
                       else if argobots_p() then "runtimeSystemArPar.a"
                       else "runtimeSystemPar.a")
                    else
                      if !region_profiling andalso gc_p() andalso tag_pairs_p() then "runtimeSystemGCTPProf.a"  else
                      if !region_profiling andalso gc_p() andalso gengc_p()     then "runtimeSystemGenGCProf.a" else
                      if !region_profiling andalso gc_p()                       then "runtimeSystemGCProf.a"    else
                      if !region_profiling                                      then "runtimeSystemProf.a"      else
                      if                           gc_p() andalso tag_pairs_p() then "runtimeSystemGCTP.a"      else
                      if                           gc_p() andalso gengc_p()     then "runtimeSystemGenGC.a"     else
                      if                           gc_p()                       then "runtimeSystemGC.a"        else
                      if tag_values()                     andalso tag_pairs_p() then
                        die "no runtime system supports tagging of values with tagging of pairs"                else
                      if tag_values()                                           then "runtimeSystemTag.a"       else
                      "runtimeSystem.a"
              in !Flags.install_dir ## "lib" ## file()
              end
    in
       val link_files_with_runtime_system = link_files_with_runtime_system0 path_to_runtime
    end


    local
      val region_profiling = Flags.is_on0 "region_profiling"
      val recompile_basislib = Flags.is_on0 "recompile_basislib"
      val tag_pairs_p = Flags.is_on0 "tag_pairs"
      val gc_p = Flags.is_on0 "garbage_collection"
      val gengc_p = Flags.is_on0 "generational_garbage_collection"
      val region_inference = Flags.is_on0 "region_inference"
    in
      fun maybe_prefix_RI s =
          if region_inference() then "RI_" ^ s
          else s

        (* Remember also to update RepositoryFinMap in Common/Elaboration.sml *)
      fun mlbdir () =
          let val subdir =
              if recompile_basislib() then "Scratch"   (* avoid overwriting other files *)
              else
                  case (gengc_p(),gc_p(), region_profiling(), tag_pairs_p()) of
                      (false,     true,   true,               false) => maybe_prefix_RI "GC_PROF"
                    | (false,     true,   false,              false) => maybe_prefix_RI "GC"
                    | (false,     true,   true,               true)  => maybe_prefix_RI "GC_TP_PROF"
                    | (false,     true,   false,              true)  => maybe_prefix_RI "GC_TP"
                    | (true,      true,   true,               false) => maybe_prefix_RI "GENGC_PROF"
                    | (true,      true,   false,              false) => maybe_prefix_RI "GENGC"
                    | (true,      _,      _,                  _)     => die "Illegal combination of generational garbage collection and tagged pairs"
                    | (false,     false,  true,               _)     => maybe_prefix_RI "PROF"
                    | (false,     false,  false,              _)     => if region_inference() then "RI"
                                                                        else "NOGC"
              val subdir = if parallelism_p() then
                             if par_alloc_unprotected_p() then
                               subdir ^ "_PAR0"
                             else subdir ^ "_PAR"
                           else subdir
              val subdir = case Flags.get_string_entry "mlb-subdir" of
                               "" => subdir
                             | x => if CharVector.all Char.isAlphaNum x then subdir ^ "_" ^ x
                                    else subdir
          in "MLB" ## subdir
          end
    end

    val pu_linkinfo =
        let val pu_labels = Pickle.listGen Labels.pu
            val pu_pair = Pickle.pairGen(pu_labels,pu_labels)
        in Pickle.convert (fn (c,i,e,u) => {code_label=c,imports=i,exports=e,unsafe=u},
                           fn {code_label=c,imports=i,exports=e,unsafe=u} => (c,i,e,u))
            (Pickle.tup4Gen(Labels.pu,pu_pair,pu_pair,Pickle.bool))
        end
  end
