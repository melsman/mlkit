signature OS_PROCESS =
  sig
    type status
    val success : status
    val failure : status
    val isSuccess : status -> bool
    val system : string -> status
    val atExit : (unit -> unit) -> unit
    val exit : status -> 'a
    val terminate : status -> 'a
    val getEnv : string -> string option
    val sleep : Time.time -> unit
  end

(*
Description

type status

    The status type represents various termination conditions for
    processes. On POSIX-based systems, status will typically be an
    integral value.

val success : status

    The unique status value that signifies successful termination of a
    process.

val failure : status

    A status value that signifies an error during execution of a
    process. Note that, in contrast to the success value, there may be
    other failure values.

isSuccess sts

    returns true if the status denotes success.

system cmd

    passes the command string cmd to the operating system's default
    shell to execute. It returns the termination status resulting from
    executing the command. It raises SysErr if the command cannot be
    executed.


    Note that, although this function is independent of the operating
    system, the interpretation of the string cmd depends very much on
    the underlying operating system and shell. On Unix systems, the
    default shell is "/bin/sh"; on Microsoft Windows systems, the
    default shell is the Microsoft Windows shell; on MacOS systems,
    the command is compiled and executed as an Apple script.

atExit f

    registers an action f to be executed when the current SML program
    calls exit. Actions will be executed in the reverse order of
    registration.

    Exceptions raised when f is invoked that escape it are trapped and
    ignored.  Calls in f to atExit are ignored. Calls to exit do not
    return, but should cause the remainder of the functions registered
    with atExit to be executed.  Calls to terminate (or similar
    functions such as Posix.Process.exit) will terminate the process
    immediately.

exit st

    executes all actions registered with atExit, flushes and closes
    all I/O streams opened using the Library, then terminates the SML
    process with termination status st.

        Implementation note:

        If the argument to exit comes from system or some other
        function (such as Unix.reap) returning a status value, then
        the implementation should attempt to preserve the meaning of
        the exit code from the subprocess.  For example, on a POSIX
        system, if Posix.Process.fromStatus st yields
        Posix.Process.W_EXITSTATUS v, then v should be passed to
        Posix.Process.exit after all necessary cleanup is done.

        If st does not connote an exit value, exit should act as
        though called with failure.  For example, on a POSIX system,
        this would occur if Posix.Process.fromStatus st is
        Posix.Process.W_SIGNALED or Posix.Process.W_STOPPED.

terminate st

    terminates the SML process with termination status st, without
    executing the actions registered with atExit or flushing open I/O
    streams.

getEnv s

    returns the value of the environment variable s, if
    defined. Otherwise, it returns NONE.

    An environment is associated with each SML process, modeled as a
    list of pairs of strings, corresponding to name-value pairs. (The
    way the environment is established depends on the host operating
    system.) The getEnv function scans the environment for a pair
    whose first component equals s. If successful, it returns the
    second component.

sleep t

    suspends the calling process for the time specified by t. If t is
    zero or negative, then the calling process does not sleep, but
    returns immediately.  No exception is raised.
*)
