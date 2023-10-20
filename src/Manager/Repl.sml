signature REPL = sig
  val run : unit -> OS.Process.status
end

functor Repl(structure ManagerObjects : MANAGER_OBJECTS
             structure IntModules : INT_MODULES
             sharing type ManagerObjects.IntBasis = IntModules.IntBasis
             sharing type ManagerObjects.modcode = IntModules.modcode)
        : REPL =
struct

structure PE = ParseElab
structure ME = ModuleEnvironments
structure IB = InfixBasis
structure EB = ME.B
structure MO = ManagerObjects
structure IM = IntModules
structure B = MO.Basis
structure PP = PrettyPrint
structure ModCode = MO.ModCode
structure CEnv = CompilerEnv
structure Ty = StatObject.Type

type Basis = MO.Basis
type modcode = MO.modcode

(* -------------------------------------------
 * Logging
 * ------------------------------------------- *)

fun die (s:string) : 'a = raise Fail ("Internal Error - Repl: " ^ s)
fun log (s:string) : unit = TextIO.output (!Flags.log, s)
fun chat s = if !Flags.chat then log (s ^ "\n") else ()
fun chatf f = if !Flags.chat then log (f() ^ "\n") else ()

fun pr_st (st) : unit = PP.outputTree (print, st, 120)

(* -------------------------------------------
 * Debugging and reporting
 * ------------------------------------------- *)

fun print_error_report report = Report.print' report (!Flags.log)
fun print_result_report report = (Report.print' report (!Flags.log);
                                  Flags.report_warnings ())

val debug_p = Flags.is_on0 "debug_compiler"

fun debug s =
    if debug_p() then print("[REPL DEBUG: " ^ s ^ "]\n")
    else ()

val print_post_elab_ast = Flags.is_on0 "print_post_elab_ast"

fun maybe_print_topdec s topdec =
    if print_post_elab_ast() orelse debug_p() then
      let val _ = print (s ^ ":\n")
	  val st = PostElabTopdecGrammar.layoutTopdec topdec
      in pr_st st
      end
    else ()

fun add_longstrid longstrid {funids, sigids, longstrids, longtycons, longvids} =
    let val longstrids = longstrid::longstrids
    in {funids=funids, sigids=sigids, longstrids=longstrids,
        longtycons=longtycons, longvids=longvids}
    end

val intinfrep = StrId.mk_LongStrId ["IntInfRep"]

fun fid_topdec a = FreeIds.fid_topdec a
fun opacity_elimination a = OpacityElim.opacity_elimination a

(* -------------------------------------------
 * Child Protocol - commands and replies
 * (see also src/Runtime/Repl.c)
 * ------------------------------------------- *)

local
  fun send_cmd (fd:Posix.FileSys.file_desc, cmd:string) : unit =
      let val cmd_bytes = Word8VectorSlice.full(Byte.stringToBytes cmd)
          val () = debug ("sending " ^ cmd)
      in if Posix.IO.writeVec(fd,cmd_bytes) <> Word8VectorSlice.length cmd_bytes then
           die ("send_cmd: failed to send whole cmd '" ^ cmd ^ "'")
         else ()
      end
in
  fun send_LOADRUN (fd, lib) : unit =
      send_cmd (fd, "LOADRUN " ^ lib ^ ";")

  fun send_PRINT (fd, ty, lab) : unit =
      send_cmd (fd, "PRINT " ^ ty ^ " " ^ lab ^ ";")
end

datatype loadrun_msg = EXN | DONE

fun receive_EXN_or_DONE (fd:Posix.FileSys.file_desc) : loadrun_msg =
    let val msg_exn = "EXN;"
        val msg_done = "DONE;"
        fun try_msg msg s =
            let val m = Posix.IO.readVec (fd, size msg - size s)
                val s = s ^ Byte.bytesToString m
            in if size s < size msg_exn then try_msg msg s
               else if s = msg then NONE
               else SOME s
            end
    in case try_msg msg_exn "" of
           NONE => EXN
         | SOME s =>
           case try_msg msg_done s of
               NONE => DONE
             | SOME s => raise Fail ("receive_EXN_or_DONE: received '" ^ s
                                     ^ "' - expected '" ^ msg_exn
                                     ^ "' or '" ^ msg_done ^ "'")
    end

fun receive_DONE (fd:Posix.FileSys.file_desc) : unit =
    let val msg = "DONE;"
        fun loop s0 =
            let val m = Posix.IO.readVec (fd, size msg)
                val s = s0 ^ Byte.bytesToString m
            in if size s < size msg then loop s
               else if s = msg then ()
               else raise Fail ("receive_DONE: received '" ^ s
                                ^ "' - expected '" ^ msg ^ "'")
            end
    in loop ""
    end

fun receive_STR (fd:Posix.FileSys.file_desc) : string option =
    let fun eat msg s0 =
            let val m = Posix.IO.readVec (fd, size msg)
                val s = s0 ^ Byte.bytesToString m
            in if size s < size msg then eat msg s
               else if s = msg then ()
               else raise Fail ("receive_STR: received '" ^ s
                                ^ "' - expected '" ^ msg ^ "'")
            end
        fun read_int a =
            let val m = Posix.IO.readVec (fd, 1)
                val s = Byte.bytesToString m
            in if size s < 1 then read_int a
               else case Int.fromString s of
                        SOME i => read_int (a*10+i)
                      | NONE =>
                        if s = " " then a else
                        raise Fail ("receive_STR: received an '"
                                    ^ s ^ "'")
            end
        fun read_string i s0 =
            let val m = Posix.IO.readVec (fd, i)
                val s = s0 ^ Byte.bytesToString m
            in if size s < i then read_string (i-size s) s
               else if size s = i then s
               else raise Fail ("receive_STR: received more than asked for")
            end
        val () = eat "STR " ""
        val i = read_int 0
        val s = read_string i ""
        val () = eat ";" ""
    in SOME s
    end handle Fail msg =>
               ( print ("FAIL: " ^ msg ^ "\n")
               ; NONE)

(* -------------------------------------------
 * Value Rendering
 * ------------------------------------------- *)

type rp = {command_pipe:Posix.IO.file_desc,reply_pipe:Posix.IO.file_desc,pid:Posix.Process.pid}

fun retrieve (rp:rp) B t longid =
    case MO.retrieve_longid B longid of
        MO.VAR lab =>
        ( send_PRINT (#command_pipe rp, t, "D." ^ lab);
          case receive_STR(#reply_pipe rp) of
              SOME s => s
            | NONE => "none" )
      | MO.STR s => s
      | MO.UNKN => "unknown"

fun str_t t =
    if Ty.eq(t,Ty.IntDefault()) then SOME "int"
    else if Ty.eq(t,Ty.Bool) then SOME "bool"
    else if Ty.eq(t,Ty.Real) then SOME "real"
    else if Ty.eq(t,Ty.Char) then SOME "char"
    else if Ty.eq(t,Ty.Int31) then SOME "int31"
    else if Ty.eq(t,Ty.Int32) then SOME "int32"
    else if Ty.eq(t,Ty.Int63) then SOME "int63"
    else if Ty.eq(t,Ty.Int64) then SOME "int64"
    else if Ty.eq(t,Ty.Word8) then SOME "word8"
    else if Ty.eq(t,Ty.WordDefault()) then SOME "word"
    else if Ty.eq(t,Ty.Word31) then SOME "word31"
    else if Ty.eq(t,Ty.Word32) then SOME "word32"
    else if Ty.eq(t,Ty.Word63) then SOME "word63"
    else if Ty.eq(t,Ty.Word64) then SOME "word64"
    else if Ty.eq(t,Ty.String) then SOME "string"
    else NONE

fun render rp B (strids,id,sch) =
    let val longid = Ident.implode_LongId(strids,id)
        val (tvs,rvs,t) = StatObject.TypeScheme.to_TyVars_and_Type sch
    in if List.null tvs then
         if Ty.is_Arrow t then "fn"
         else if Ty.eq(Ty.Unit, t) then "()"
         else case str_t t of
                  SOME t => retrieve rp B t longid
                | NONE => ".."
       else "fun"
    end

(* --------------------------------------------------
 * Command management. Commands are prefixed with
 * a ':' and terminated with a ';'
 * ------------------------------------------------- *)

local
  fun mem s ss = List.exists (fn s' => s=s') ss

  fun is_flag0 s =
      List.exists (fn opt => #kind opt = NONE (* no args *)
                             andalso (mem s (#long opt) orelse mem s (#short opt)))
                  (Flags.getOptions_noneg())

  fun is_flag s =
      List.exists (fn opt => mem s (#long opt) orelse mem s (#short opt))
                  (Flags.getOptions_noneg())

  fun err s =
      print ("** [" ^ s ^ "]\n")

  fun print_help () =
      print ("The REPL accepts commands and toplevel declarations. Commands\n\
             \are of the form:\n\
             \\n\
             \  cmd ::= :help            -- general help\n\
             \        | :set flag [arg]  -- set flag [maybe with arg]\n\
             \        | :unset flag      -- unset the flag\n\
             \        | :help flag       -- get help about a flag\n\
             \        | :quit            -- quit\n\
             \        | :flags           -- list all flags\n\
             \")

  fun print_flags () =
      let fun pr_flag fl =
              case #long fl of
                  [n] =>
                  let val shorts =
                          case #short fl of
                              nil => ""
                            | shorts => " [" ^ String.concatWith "," shorts ^ "]"
                  in case #kind fl of
                         NONE => SOME(n ^ shorts)
                       | SOME arg => SOME(n ^ " (" ^ arg ^ ")" ^ shorts)
                  end
                | _ => NONE
          val flags = List.mapPartial pr_flag (Flags.getOptions_noneg())
      in
        print("Available flags:\n" ^ Flags.help_all_nodash_noneg())
      end

  fun print_flag_help k =
      if is_flag k then print (Flags.help_nodash k)
      else err ("Invalid flag '" ^ k ^ "'")
in
exception Quit
fun process_cmd (cmd:string) =
    let val ts = String.tokens Char.isSpace cmd
    in case ts of
           [":set", s] => if is_flag0 s then Flags.turn_on s
                          else err ("Invalid flag '" ^ s ^ "'")
         | [":unset", s] => if is_flag0 s then Flags.turn_off s
                            else err ("Invalid flag '" ^ s ^ "'")
         | [":help"] => print_help()
         | [":help", fl] => print_flag_help fl
         | [":flags"] => print_flags()
         | [":quit"] => raise Quit
         | _ => err ("Invalid command '" ^ cmd ^ "'")
    end
end

(* -------------------------------------------
 * The REPL
 * ------------------------------------------- *)

fun do_exit (rp:rp) status =
    ( print "Exiting\n"
    ; Posix.Process.kill (Posix.Process.K_PROC (#pid rp), Posix.Signal.kill)
    ; Posix.Process.wait()
    ; OS.Process.exit status
    )

fun repl (stepno, B:Basis, state, rp:rp, libs_acc) : OS.Process.status =
    let val state =
            let fun loop state =
                    case PE.colonLine (PE.stripSemiColons state) of
                        SOME(cmd,state) => ( process_cmd cmd
                                           ; loop state
                                           )
                      | NONE => state
            in loop state
            end
        val absprjid = ME.mk_absprjid "repl"
        val smlfile = "stdin_" ^ Int.toString stepno
        val (infB, elabB, _, _) = B.un B
        val _ = Name.bucket := []
        val base = absprjid
        val _ = Name.baseSet base
    in case PE.parse_elab_stdin {infB=infB, elabB=elabB,
                                 absprjid=absprjid, state=state} of
           (SOME state', PE.SUCCESS {doreport,infB=infB',elabB=elabB',topdec}) =>
           let val () = debug "elaboration succeeded"
               val _ = chat "[finding free identifiers begin...]"
               val freelongids = add_longstrid intinfrep (fid_topdec topdec)
               val _ = chat "[finding free identifiers end...]"
               val (B_im,_) = B.restrict(B,freelongids)
               val (_,_,oE_im,intB_im) = B.un B_im

               (* Setting up for generation of second export basis (eb1) *)
               val names_elab = !Name.bucket
               val _ = List.app Name.mk_rigid names_elab
               val _ = Name.bucket := []
               val _ = Name.baseSet (base ^ "1")

               val _ = chat "[opacity elimination begin...]"
               val (topdec', oE') = opacity_elimination(oE_im, topdec)
               val _ = chat "[opacity elimination end...]"

	       val _ = maybe_print_topdec "AST after opacity elimination" topdec

               val _ = chat "[interpretation begin...]"
               val functor_inline = true
               val (intB', modc) = IM.interp(functor_inline, absprjid, intB_im, topdec', smlfile)
               val names_int = !Name.bucket
               val _ = List.app Name.mk_rigid names_int
               val _ = Name.bucket := []
               val _ = chat "[interpretation end...]"

               (* compute result basis *)
               val B' = B.mk(infB',elabB',oE',intB')

               (* create a shared library *)
               val modc = ModCode.emit (absprjid,modc)
               val sofile = OS.Path.concat(MO.mlbdir(), "lib" ^ smlfile ^ ".so")
               val () = ModCode.mk_sharedlib (modc, libs_acc, smlfile, sofile)

               (* send command to load and run the shared library *)
               val () = send_LOADRUN(#command_pipe rp,sofile)

           in case receive_EXN_or_DONE(#reply_pipe rp) of
                  EXN =>
                  ( repl (stepno+1,         (* No reporting! *)
                          B,                (* No added toplevel binding *)
                          PE.begin_stdin(), (* Clear the state *)
                          rp,
                          libs_acc)         (* Nothing from this unit to link to later, I think *)
                  )
                | DONE =>
                  let val B'' = B.plus(B,B')
                  in Report.print (doreport (SOME (render rp B'')))
                   ; repl (stepno+1,
                           B'',
                           state',
                           rp,
                           smlfile::libs_acc)
                  end
           end
         | (SOME state', PE.FAILURE (report,errs)) =>
           ( Report.print report
           ; repl (stepno+1,
                   B,
                   PE.begin_stdin(),   (* Clear the state *)
                   rp,
                   libs_acc)
           )
         | (_, PE.FAILURE (report,errs)) =>
           ( Report.print report
           ; do_exit rp OS.Process.failure
           )
         | (NONE, PE.SUCCESS _) => die "repl - impossible"
    end handle Quit => do_exit rp OS.Process.success

val flags_to_block = ["regionvar", "values_64bit", "uncurrying",
    "safeLinkTimeElimination", "repository", "reml", "strip",
    "tag_pairs", "tag_values", "unbox_reals", "warn_spurious",
    "region_profiling", "recompile_basislib", "print_K_normal_forms",
    "parallelism_alloc_unprotected", "print_bit_vectors",
    "print_all_program_points", "parallelism", "output", "namebase",
    "mlb-subdir", "link_time_dead_code_elimination", "link_code",
    "libs", "libdirs", "import_basislib",
    "generational_garbage_collection", "gdb_support",
    "garbage_collection", "extra_gc_checks", "compile_only",
    "c_compiler", "assembler", "argobots", "alloc_protect_always",
    "SML_LIB", "quotation", "warn_on_parallel_puts", "mlb_path_maps",
    "load_basis_files", "disable_spurious_type_variables",
    "dangling_pointers_statistics", "dangling_pointers",
    "statistics_spurious", "type_check_lambda", "report_file_sig"]

fun run () : OS.Process.status =
    let val () = Flags.turn_on "report_file_sig"
        val () = List.app Flags.block_entry flags_to_block
        val () = if Flags.is_on "garbage_collection" then
                   ( print("Disabling garbage collection - it is not supported with the REPL\n")
                   ; Flags.turn_off "garbage_collection"
                   )
                 else ()
        val () = Flags.turn_off "messages"
        val cc = !(Flags.lookup_string_entry "c_compiler")
    in case MO.mk_repl_runtime of
           SOME f =>
           let val rt_exe = f()
               val () = debug ("wrote runtime executable " ^ rt_exe)
               (* Now, start the runtime executable in a child process,
                  but first create two named pipes *)
               val command_pipe_name = OS.Path.concat(MO.mlbdir(), "command_pipe")
               val reply_pipe_name = OS.Path.concat(MO.mlbdir(), "reply_pipe")
               val repl_logfile = OS.Path.concat(MO.mlbdir(), "repl.log")
               val () = OS.FileSys.remove command_pipe_name handle _ => ()
               val () = OS.FileSys.remove reply_pipe_name handle _ => ()
               val () = Posix.FileSys.mkfifo (command_pipe_name, Posix.FileSys.S.irwxu)
               val () = Posix.FileSys.mkfifo (reply_pipe_name, Posix.FileSys.S.irwxu)
               val childpid =
                   case Posix.Process.fork() of
                       SOME pid => pid
                     | NONE =>
                       ( Posix.Process.execp (rt_exe, [OS.Path.file rt_exe,
                                                       "-command_pipe", command_pipe_name,
                                                       "-reply_pipe", reply_pipe_name,
                                                       "-repl_logfile", repl_logfile])
                       ; OS.Process.exit OS.Process.failure (* never gets here *)
                       )
               val () = debug "created fifos"
               val command_pipe = Posix.FileSys.openf(command_pipe_name,Posix.FileSys.O_WRONLY,
                                                      Posix.FileSys.O.flags[])
                                  handle _ => die "run: failed to open command_pipe"
               val () = debug "opened command_pipe"
               val reply_pipe = Posix.FileSys.openf(reply_pipe_name,Posix.FileSys.O_RDONLY,
                                                    Posix.FileSys.O.flags[])
                                handle _ => die "run: failed to open reply_pipe"
               val () = debug "opened reply_pipe"
               val rp = {command_pipe=command_pipe,reply_pipe=reply_pipe,pid=childpid}
           in repl (0, B.initial(), PE.begin_stdin(), rp, ["runtime"])
           end
         | NONE => die "run - not possible to build runtime"
    end

end
