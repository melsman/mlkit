signature POSIX =
  sig
    structure Error : POSIX_ERROR
    structure Signal : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
      where type signal = Signal.signal
    structure ProcEnv : POSIX_PROCENV
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
(*    structure TTY : POSIX_TTY
      where type pid = Process.pid
      where type file_desc = ProcEnv.file_desc *)
  end
