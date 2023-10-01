(**  Functionality for interacting with the operating system. *)

signature OS =
  sig
    type syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror option

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO
  end

(**

[type syserror] Type of underlying system errors.

[exception SysErr(s,erropt)] Represents a system error.

[errorMsg err] Returns a string explaining the error message
corresponding to a system error code (e.g., as found in a SysErr
exception). The precise form of the returned string is operating
system dependent.

[structure FileSys] File system operations for the operating system.

[structure Path] Path management operations for the operating system.

[structure Process] Operating system processes.

[structure IO] Input and output operations for the operating system.

*)

(** SigDoc *)
structure OS : OS =
  struct
    type syserror = OS.syserror
    exception SysErr = OS.SysErr
    fun errorMsg (err : int) : string = OS.errorMsg err
    fun errorName (err : int) : string = OS.errorName err
    fun syserror (err : string) : syserror option = OS.syserror err

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
