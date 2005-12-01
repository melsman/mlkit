signature POSIX_IO = 
  sig
    eqtype file_desc
    eqtype pid
(*    eqtype open_mode *)

    datatype open_mode =
        O_RDONLY
      | O_WRONLY
      | O_RDWR

    val close : file_desc -> unit
    val dup : file_desc -> file_desc
    val dupfd : {old : file_desc, base : file_desc} -> file_desc
    val pipe : unit -> {infd : file_desc, outfd : file_desc}


  end
