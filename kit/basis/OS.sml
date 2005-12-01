
signature OS = 
  sig
    type syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO
  end

(*  Various functions for interacting with the operating system.

   [errorMsg err] returns a string explaining the error message system
   error code err, as found in a SysErr exception.  The precise form
   of the strings are operating system dependent.  
*)

structure OS : OS = 
  struct
    type syserror = OS.syserror
    exception SysErr = OS.SysErr
    fun errorMsg (err : int) : string = OS.errorMsg err

    structure FileSys = FileSys
    structure Path = Path
    structure Process = Process
    structure IO : OS_IO =
      struct
        type iodesc = int
        val hash = Word.fromInt
        val compare = Int.compare
        type iodesc_kind = Word.word

        structure Kind =
          struct
            val file = 0w1
            val dir = 0w2
            val symlink = 0w4
            val tty = 0w8
            val pipe = 0w10
            val socket = 0w20
            val device = 0w40
          end
        exception Poll
      end
  end

