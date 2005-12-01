signature POSIX_PROCENV = 
  sig
    eqtype uid
    eqtype gid
    eqtype pid
    eqtype file_desc
    val sysconf : string -> SysWord.word
    val times : unit ->
                {
                  elapsed : Time.time,
                  utime : Time.time,
                  stime : Time.time,
                  cutime : Time.time,
                  cstime : Time.time
                }
    val isatty : file_desc -> bool
  end
