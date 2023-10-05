(** POSIX Modules. *)

signature POSIX =
  sig
    structure Error : POSIX_ERROR
    structure Signal : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
      where type signal = Signal.signal
    structure ProcEnv : POSIX_PROC_ENV
      where type pid = Process.pid
    structure FileSys : POSIX_FILE_SYS
      where type file_desc = ProcEnv.file_desc
      where type uid = ProcEnv.uid
      where type gid = ProcEnv.gid
    structure IO : POSIX_IO
      where type pid = Process.pid
      where type file_desc = ProcEnv.file_desc
      where type open_mode = FileSys.open_mode
    structure SysDB : POSIX_SYS_DB
      where type uid = ProcEnv.uid
      where type gid = ProcEnv.gid
  end

(**

[structure Error] Symbolic names for POSIX errors.

[structure Signal] Symbolic names of POSIX signals.

[structure Process] POSIX process operations.

[structure ProcEnv] Operations for accessing POSIC process
environments.

[structure FileSys] POSIX file system operations.

[structure IO] Posix IO operations.

[structure SysDB] Operations on the POSIX user and group database.

*)
