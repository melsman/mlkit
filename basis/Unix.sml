(* Originally from SML/NJ sources, through MLton.  Very slightly
modified for MLKit. *)

structure Unix: UNIX =
  struct

    structure OSP = OS.Process
    structure PP = Posix.Process
    structure PPE = Posix.ProcEnv
    structure PFS = Posix.FileSys
    structure PIO = Posix.IO
    structure SS = Substring

    type signal = Posix.Signal.signal
    datatype exit_status = datatype Posix.Process.exit_status
    val fromStatus = Posix.Process.fromStatus

    datatype 'a str = FD of PFS.file_desc | STR of 'a * ('a -> unit)
    fun close str =
      case str of
        FD file_desc => PIO.close file_desc
      | STR (str, close) => close str

    datatype ('a, 'b) proc = PROC of {pid: PP.pid,
                                      status: OSP.status option ref,
                                      ins: 'a str ref,
                                      outs: 'b str ref}

    fun executeInEnv (cmd, argv, env) =
          let
             val p1 = PIO.pipe ()
             val p2 = PIO.pipe ()
             fun closep () = (PIO.close (#outfd p1);
                              PIO.close (#infd p1);
                              PIO.close (#outfd p2);
                              PIO.close (#infd p2))
             val base = SS.string(SS.taker (fn c => c <> #"/") (SS.full cmd))
             fun startChild () =
                case PP.fork () of
                   SOME pid => pid (* parent *)
                 | NONE => let
                              val oldin = #infd p1
                              val oldout = #outfd p2
                              val newin = PFS.stdin
                              val newout = PFS.stdout
                           in
                              PIO.close (#outfd p1);
                              PIO.close (#infd p2);
                              if (oldin = newin) then ()
                              else (PIO.dup2{old = oldin, new = newin};
                                    PIO.close oldin);
                              if (oldout = newout) then ()
                              else (PIO.dup2{old = oldout, new = newout};
                                    PIO.close oldout);
                              PP.exece (cmd, base :: argv, env)
                           end
             (* end case *)
             val _ = TextIO.flushOut TextIO.stdOut
             val pid = (startChild ()) handle ex => (closep(); raise ex)
          in
             (* close the child-side fds *)
             PIO.close (#outfd p2);
             PIO.close (#infd p1);
             (* set the fds close on exec *)
             PIO.setfd (#infd p2, PIO.FD.flags [PIO.FD.cloexec]);
             PIO.setfd (#outfd p1, PIO.FD.flags [PIO.FD.cloexec]);
             PROC {
                   pid = pid,
                   status = ref NONE,
                   ins = ref (FD (#infd p2)),
                   outs = ref (FD (#outfd p1))
                   }
          end

    fun execute (cmd, argv) = executeInEnv (cmd, argv, PPE.environ())

    local
      fun mkInstreamOf (newIn, closeIn) (PROC {ins, ...}) =
        case !ins of
          FD file_desc => let val str' = newIn (file_desc, "<process>")
                          in ins := STR (str', closeIn); str'
                          end
        | STR (str, _) => str
      fun mkOutstreamOf (newOut, closeOut) (PROC {outs, pid, ...}) =
        case !outs of
          FD file_desc => let val str' = newOut (file_desc, "<process>")
                          in outs := STR (str', closeOut); str'
                          end
        | STR (str, _) => str
    in
      fun textInstreamOf proc =
         mkInstreamOf (TextIO.newIn, TextIO.closeIn) proc
      fun textOutstreamOf proc =
         mkOutstreamOf (TextIO.newOut, TextIO.closeOut) proc
      fun binInstreamOf proc =
         mkInstreamOf (BinIO.newIn, BinIO.closeIn) proc
      fun binOutstreamOf proc =
         mkOutstreamOf (BinIO.newOut, BinIO.closeOut) proc
    end
    fun streamsOf pr = (textInstreamOf pr, textOutstreamOf pr)

    fun reap (PROC{pid, status, ins, outs}) =
      case !status of
        SOME status => status
      | NONE => let
                  val _ = close (!ins)
                  val _ = close (!outs)
                  val (_, st) = PP.waitpid (PP.W_CHILD pid, [])

                  (* XXX: this function must return an
                  OS.Process.status, but waitpid returns a
                  Posix.Process.exit_status.  We make the lossy
                  assumption of turning W_EXITED into a success and
                  everything else a failure. *)

                  val st' =
                      case st of
                          W_EXITED => OS.Process.success
                       |  _ => OS.Process.failure

                in
                  status := SOME st'; st'
                end

    fun kill (PROC{pid, ...}, signal) = PP.kill (PP.K_PROC pid, signal)

    fun exit st = OSP.exit (Word8.toInt st)
  end (* structure Unix *)
