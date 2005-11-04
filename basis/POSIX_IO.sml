signature POSIX_IO = 
  sig
    eqtype file_desc
    eqtype open_mode
    eqtype pid

    val close : file_desc -> unit
    val dup : file_desc -> file_desc
    val dupfd : {old : file_desc, base : file_desc} -> file_desc
  end
