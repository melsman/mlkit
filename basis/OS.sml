
signature OS = 
  sig
    type syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
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
  end

