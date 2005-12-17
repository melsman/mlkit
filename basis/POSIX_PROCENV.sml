signature POSIX_PROCENV = 
  sig
    eqtype uid
    eqtype gid
    eqtype pid
    eqtype file_desc

    val wordToUid : SysWord.word -> uid
    val uidToWord : uid -> SysWord.word
    val wordToGid : SysWord.word -> gid
    val gidToWord : gid -> SysWord.word

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
