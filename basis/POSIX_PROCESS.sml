(** POSIX process operations.

The structure Posix.Process describes the primitive POSIX operations
dealing with processes, as described in Section 3 of the POSIX
standard 1003.1,1996.

*)

signature POSIX_PROCESS =
  sig
    eqtype signal
    eqtype pid
    val wordToPid : SysWord.word -> pid
    val pidToWord : pid -> SysWord.word
    val fork      : unit -> pid option
    val exec      : string * string list -> 'a
    val exece     : string * string list * string list -> 'a
    val execp     : string * string list -> 'a
    datatype waitpid_arg
      = W_ANY_CHILD
      | W_CHILD of pid
      | W_SAME_GROUP
      | W_GROUP of pid
    datatype exit_status
      = W_EXITED
      | W_EXITSTATUS of Word8.word
      | W_SIGNALED of signal
      | W_STOPPED of signal
    val fromStatus : OS.Process.status -> exit_status
    structure W : sig
        include BIT_FLAGS
        val untraced : flags
      end
    val wait       : unit -> pid * exit_status
    val waitpid    : waitpid_arg * W.flags list -> pid * exit_status
    val waitpid_nh : waitpid_arg * W.flags list -> (pid * exit_status) option
    val exit       : Word8.word -> 'a

    datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid

    val kill  : killpid_arg * signal -> unit
    val alarm : Time.time -> Time.time
    val pause : unit -> unit
    val sleep : Time.time -> Time.time
  end

(**

[eqtype signal] The type of a POSIX signal, an asynchronous
notification of an event.

[eqtype pid] The type of a process ID, used as an identifier for an
operating system process.

[wordToPid w]
[pidToWord pid]

These functions convert between a process ID and the integer
representation used by the operating system. Note that there is no
validation that a pid value generated using wordToPid is legal on the
given system or that it corresponds to a currently running process.

[fork()] This function creates a new process. The new child process is
a copy of the calling parent process. After execution of fork, both
the parent and child process execute independently, but share various
system resources. Upon successful completion, fork returns NONE in the
child process, and the pid of the child in the parent process. It
raises OS.SysErr on failure.

[exec (path, args)]
[exece (path, args, env)]
[execp (file, args)]

These functions replace the current process image with a new process
image. There is no return from a successful call, as the calling
process image is overlaid by the new process image. In the first two
forms, the path argument specifies the pathname of the executable
file. In the last form, if file contains a slash character, it is
treated as the pathname for the executable file; otherwise, an
executable file with name file is searched for in the directories
specified by the environment variable PATH.

Normally, the new image is given the same environment as the calling
program. The env argument in exece allows the program to specify a new
environment.

The args argument is a list of string arguments to be passed to the
new program. By convention, the first item in args is some form of the
filename of the new program, usually the last arc in the path or
filename.

[datatype waitpid_arg] Type with constructors specifying what
processes to wait for:

    W_ANY_CHILD  : Any child process

    W_CHILD pid  : The child process with the given pid

    W_SAME_GROUP : Any child process in the same process group as the
                   calling process

    W_GROUP pid  : Any child process whose process group ID is given
                   by pid.

[datatype exit_status] These values represent the ways in which a
process might stop. They correspond to, respectively, terminate
successfully (W_EXITED), terminate with the given value
(W_EXITSTATUS), terminate upon receipt of the given signal
(W_SIGNALED), and stop upon receipt of the given signal
(W_STOPPED). The value carried by W_EXITSTATUS must never be zero.  If
an implementation provides both the Posix and Unix structures, then
the datatypes Posix.Process.exit_status and Unix.exit_status must be
the same.

[fromStatus sts] returns a concrete view of the given status.

[structure W]

    [untraced] In systems supporting job control, this flag requests
    the status of child processes that are stopped.

[wait()] This function allows a calling process to obtain status
information on any of its child processes. Execution of wait suspends
execution until status information on one of its child processes is
available. If status information is available prior to the execution
of wait, return is immediate. wait returns the process ID of the child
and its exit status.

[waitpid (procs, l)] is identical to wait except that the status is
reported only for child processes specified by procs. A set of flags l
may be used to modify the behavior of waitpid.

[waitpid_nh (procs, l)] is identical to waitpid, except that the call
does not suspend if status information for one of the children
specified by procs is not immediately available.

Rationale: In C, waitpid_nh is handled by waitpid, using an additional
flag to indicate no hanging. In SML, the semantics of waitpid_nh
indicated a different return type from that of waitpid, hence the
split into two functions.

[exit i] terminates the calling process. If the parent process is
executing a wait related call, the exit status i is made available to
it. exit does not return to the caller.  Calling exit does not flush
or close any open IO streams, nor does it call OS.Process.atExit. It
does close any open POSIX files, and performs the actions associated
with the C version of exit.

[datatype killpid_arg] Type with constructors specifying processes to
kill:

    K_PROC pid   : The process with ID pid.

    K_SAME_GROUP : All processes in the same process group as the
                   calling process.

    K_GROUP pid  : All processes in the process group specified by
                   pid.

[kill (procs, sig)] sends the signal sig to the process or group of
processes specified by procs.

[alarm t] causes the system to send an alarm signal (alrm) to the
calling process after t seconds have elapsed. If there is a previous
alarm request with time remaining, the alarm function returns a
nonzero value corresponding to the number of seconds remaining on the
previous request. Zero time is returned if there are no outstanding
calls.

[pause()] This function suspends the calling process until the
delivery of a signal that is either caught or that terminates the
process.

[sleep t] causes the current process to be suspended from execution
until either t seconds have elapsed, or until the receipt of a signal
that is either caught or that terminates the process.

*)
