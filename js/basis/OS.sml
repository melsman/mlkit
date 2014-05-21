
signature OS = 
  sig
    type syserror

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror option

    structure Path : OS_PATH
  end

structure OS :> OS = 
  struct
    type syserror = string

    exception SysErr of string * syserror option

    val errorMsg : syserror -> string = fn x => x
    val errorName : syserror -> string = fn x => x
    val syserror : string -> syserror option = fn _ => NONE

    structure Path = Path
  end

