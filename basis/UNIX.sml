signature UNIX =
  sig
    type ('a,'b) proc
    type signal
    datatype exit_status
      = W_EXITED
      | W_EXITSTATUS of Word8.word
      | W_SIGNALED of signal
      | W_STOPPED of signal
    val fromStatus : OS.Process.status -> exit_status
    val executeInEnv : string * string list * string list
                         -> ('a, 'b) proc
    val execute : string * string list -> ('a, 'b) proc
    val textInstreamOf : (TextIO.instream, 'a) proc
                           -> TextIO.instream
    val binInstreamOf  : (BinIO.instream, 'a) proc
                           -> BinIO.instream
    val textOutstreamOf : ('a, TextIO.outstream) proc
                            -> TextIO.outstream
    val binOutstreamOf  : ('a, BinIO.outstream) proc
                            -> BinIO.outstream
    val streamsOf : (TextIO.instream, TextIO.outstream) proc
                      -> TextIO.instream * TextIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status
    val kill : ('a, 'b) proc * signal -> unit
    val exit : Word8.word -> 'a
  end

(*
type ('a,'b) proc
    A type representing a handle for an operating system process.

type signal
    A Unix-like signal which can be sent to another process. Note that signal
    values must be obtained from some other structure. For example, an
    implementation providing the Posix module would probably equate the signal
    and Posix.Signal.signal types.

datatype exit_status
  = W_EXITED
  | W_EXITSTATUS of Word8.word
  | W_SIGNALED of signal
  | W_STOPPED of signal
    These values represent the ways in which a Unix process might stop. They
    correspond to, respectively, successful termination, termination with the
    given exit value, termination upon receipt of the given signal, and
    stopping upon receipt of the given signal. The value carried by
    W_EXITSTATUS will be non-zero.

    If an implementation provides both the Posix and Unix structures, then
    Posix.Process.exit_status and exit_status must be the same type.

fromStatus sts
    returns a concrete view of the given status.

executeInEnv (cmd, args, env)
    asks the operating system to execute the program named by the string cmd
    with the argument list args and the environment env. The program is run as
    a child process of the calling program; the return value of this function
    is an abstract proc value naming the child process.  Strings in the env
    list typically have the form "name=value" (see OS.Process.getEnv).

    The executeInEnv function raises the OS.SysErr exception if it fails.
    Reasons for failure include insufficient memory, too many processes, and
    the case where cmd does not name an executable file. If the child process
    fails to execute the command (i.e., the execve call fails), then it should
    exit with a status code of 126.

execute (cmd, args)
    asks the operating system to execute the program named by the string cmd
    with the argument list args. The program is run as a child process of the
    calling program and it inherits the calling process's environment; the
    return value of this function is an abstract proc value naming the child
    process. The failure semantics of this function are the same as for
    executeInEnv.

    For implementations providing the Posix modules, this function is
    equivalent to

          fun execute (cmd, args) =
	        executeInEnv (cmd, args, Posix.ProcEnv.environ ())
          
textInstreamOf pr
binInstreamOf pr
    These return a text or binary instream connected to the standard output
    stream of the process pr.

    Note that multiple calls to these functions on the same proc value will
    result in multiple streams that all share the same underlying open file
    descriptor, which can lead to unpredictable effects because of the state
    inherent in file descriptors.

textOutstreamOf pr
binOutstreamOf pr
    These return a text or binary outstream connected to the standard input
    stream of the process pr.

    Note that multiple calls to these functions on the same proc value will
    result in multiple streams that all share the same underlying open file
    descriptor, which can lead to unpredictable effects due to buffering.

streamsOf pr
    returns a pair of input and output text streams associated with pr. This
    function is equivalent to (textInstream pr, textOutstream pr) and is
    provided for backward compatibility.

reap pr
    closes the input and output streams associated with pr, and then suspends
    the current process until the system process corresponding to pr
    terminates. It returns the exit status given by pr when it terminated. If
    reap is applied again to pr, it should immediately return the previous exit
    status.

        Implementation note:

        Typically, one cannot rely on the underlying operating system to
        provide the exit status of a terminated process after it has done so
        once. Thus, the exit status probably needs to be cached. Also note that
        reap should not return until the process being monitored has
        terminated. In particular, implementations should be careful not to
        return if the process has only been suspended. 

kill (pr,s)
    sends the signal s to the process pr.

exit st
    executes all actions registered with OS.Process.atExit, flushes and closes
    all I/O streams opened using the Library, then terminates the SML process
    with termination status st.

Discussion

    Note that the interpretation of the string cmd in the execute and
    executeInEnv functions depends very much on the underlying operating
    system. Typically, the cmd argument will be a full pathname.

    The semantics of Unix necessitates that processes that have terminated need
    to be reaped. If this is not done, information concerning the dead process
    continues to reside in system tables. Thus, a program using execute or
    executeInEnv should invoke reap on any subprocess it creates.

*)
