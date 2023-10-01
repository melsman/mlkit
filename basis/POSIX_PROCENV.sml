(** Operations for accessing POSIC process environments.

The structure Posix.ProcEnv specifies functions, as described in
Section 4 of the POSIX standard 1003.1,1996[CITE], which provide
primitive POSIX access to the process environment.

*)

signature POSIX_PROCENV =
  sig
    eqtype pid
    eqtype uid
    eqtype gid
    eqtype file_desc
    val uidToWord : uid -> SysWord.word
    val wordToUid : SysWord.word -> uid
    val gidToWord : gid -> SysWord.word
    val wordToGid : SysWord.word -> gid
    val getpid    : unit -> pid
    val getppid   : unit -> pid
    val getuid    : unit -> uid
    val geteuid   : unit -> uid
    val getgid    : unit -> gid
    val getegid   : unit -> gid
    val setuid    : uid -> unit
    val setgid    : gid -> unit
    val getgroups : unit -> gid list
    val getlogin  : unit -> string
    val getpgrp   : unit -> pid
    val setsid    : unit -> pid
    val setpgid   : {pid : pid option, pgid : pid option} -> unit
    val uname     : unit -> (string * string) list
    val time      : unit -> Time.time
    val times     : unit -> { elapsed : Time.time,
                              utime : Time.time,
                              stime : Time.time,
                              cutime : Time.time,
                              cstime : Time.time
                            }
    val getenv    : string -> string option
    val environ   : unit -> string list
    val ctermid   : unit -> string
    val ttyname   : file_desc -> string
    val isatty    : file_desc -> bool
    val sysconf   : string -> SysWord.word
  end

(**

[eqtype pid] The type of a process ID, used as an identifier for an
operating system process.

[eqtype uid] The type of a user identifier.

[eqtype gid] The type of a group identifier.

[eqtype file_desc] The type of an open file descriptor.

[uidToWord uid]
[wordToUid w]

These functions convert between an abstract user ID and an underlying
unique unsigned integer. Note that wordToUid does not ensure that it
returns a valid uid.

[gidToWord gid]
[wordToGid w]

These functions convert between an abstract group ID and an underlying
unique unsigned integer. Note that wordToGid does not ensure that it
returns a valid gid.

[getpid()]
[getppid()]

Returns the process ID and the parent process ID, respectively, of the
calling process.

[getuid()]
[geteuid()]

Returns the real and effective user IDs, respectively, of the calling
process.

[getgid()]
[getegid()]

Returns the real and effective group IDs, respectively, of the calling
process.

[setuid u] sets the real user ID and effective user ID to u.

[setgid g] sets the real group ID and effective group ID to g.

[getgroups()] Returns the list of supplementary group IDs of the
calling process.

[getlogin()] Returns the user name associated with the calling
process, i.e., the login name associated with the calling process.

[getpgrp()] Returns the process group ID of the calling process.

[setsid()] This function creates a new session if the calling process
is not a process group leader, and returns the process group ID of the
calling process.

[setpgid (SOME pid, SOME pgid)]
[setpgid (NONE, SOME pgid)]
[setpgid (SOME pid, NONE)]
[setpgid (NONE, NONE)]

The first (second) usage sets the process group ID of the process with
process ID pid (of the calling process, respectively) to pgid. In the
third (fourth) usage, the process with process ID pid (the calling
process, respectively) becomes a process group leader.

[uname()] Returns a list of name-value pairs including, at least, the
names: "sysname", "nodename", "release", "version", and "machine". (A
POSIX implementation may provide additional values beyond this set.)
The respective values are strings that describe the named system
component.

[time()] Returns the elapsed wall time since the Epoch.

[times()] Returns a record containing the wall time (elapsed), user
time (utime), system time (stime), user CPU time of terminated child
processes (cutime), and system CPU time of terminated child processes
(cstime), for the calling process.

[getenv name] searches the environment list for a string of the form
name=value and returns SOME(value) if name is present; it returns NONE
if name is not present. This is equivalent to OS.Process.getEnv.

[environ()] Returns the environment of the calling process as a list
of strings.

[ctermid()] Rerturns a string that represents the pathname of the
controlling terminal for the calling process.

[ttyname fd] produces a string that represents the pathname of the
terminal associated with file descriptor fd. It raises OS.SysErr if fd
does not denote a valid terminal device.

[isatty fd] returns true if fd is a valid file descriptor associated
with a terminal. Note that isatty will return false if fd is a bad
file descriptor.

[sysconf s] returns the integer value for the POSIX configurable
system variable s. It raises OS.SysErr if s does not denote a
supported POSIX system variable.  The properties required by POSIX are
described below. This list is a minimal set required for POSIX
compliance, and an implementation may extend it with additional
properties.

    "ARG_MAX"     Maximum length of arguments, in bytes, for the
                  functions Posix.Process.exec, Posix.Process.exece,
                  and Posix.Process.execp. This also applies to
                  environment data.

    "CHILD_MAX"   Maximum number of concurrent processes
                  associated with a real user ID.

    "CLK_TCK"     Number of clock ticks per second.

    "NGROUPS_MAX" Maximum number of supplementary group IDs
                  associated with a process, in addition to the
                  effective group ID.

    "OPEN_MAX"    Maximum number of files that one process can
                  have open concurrently.

    "STREAM_MAX"  Maximum number of streams that one process can
                  have open concurrently.

    "TZNAME_MAX"  Maximum number bytes allowed for a time zone
                  name.

    "JOB_CONTROL" Non-zero if the implementation supports job
                  control.

    "SAVED_IDS"   Non-zero if each process has a saved set-user-ID
                  and saved set-group-ID.

    "VERSION"     A version number.

Consult Section 4.8 of POSIX standard 1003.1,1996 for additional
information. Note that a property in SML has the same name as the
property in C, but without the prefix "_SC_".

*)
