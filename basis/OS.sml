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

        val iodToFD = PosixStat.iodToFD

        structure ST = PosixStat.ST

        fun kind iod : iodesc_kind =
            case iodToFD iod of
                SOME fd =>
                let val s = PosixStat.fstat fd
                in if ST.isReg s then Kind.file
                   else if ST.isDir s then Kind.dir
                   else if ST.isChr s then Kind.tty
                   else if ST.isBlk s then Kind.device
                   else if ST.isLink s then Kind.symlink
                   else if ST.isFIFO s then Kind.pipe
                   else if ST.isSock s then Kind.socket
                   else raise SysErr ("kind of IO descriptor unknown",NONE)
                end
              | NONE => raise SysErr ("unknown IO descriptor",NONE)

        type poll_info = {iod:iodesc,pri:bool,rd:bool,wr:bool}  (* alphabetical order for C-ffi *)
        type poll_desc = poll_info

        (* create a polling operation on the given descriptor; note that
         * not all I/O devices support polling, but for the time being, we
         * don't test for this.
         *)
        fun pollDesc iod = SOME {iod=iod, pri=false, rd=false, wr=false}

        (* return the I/O descriptor that is being polled *)
        fun pollToIODesc (pd: poll_desc) = #iod pd

        exception Poll

        (* set polling events; if the polling operation is not appropriate
         * for the underlying I/O device, then the Poll exception is raised.
         *)
        fun pollIn ({iod, pri, wr, ...}: poll_desc) : poll_desc =
            {iod=iod, pri=pri, rd=true, wr=wr}
        fun pollOut ({iod, pri, rd, ...}: poll_desc) : poll_desc =
            {iod=iod, pri=pri, rd=rd, wr=true}
        fun pollPri ({iod, rd, wr, ...}: poll_desc) : poll_desc =
            {iod=iod, pri=true, rd=rd, wr=wr}

        fun getCtx () : foreignptr = prim("__get_ctx",())

        exception Poll
        fun poll (pds: poll_desc list, timeout:Time.time option) : poll_info list =
            let val tm_out = case timeout of
                                 NONE => ~1
                               | SOME t => Int.fromLarge(Time.toMilliseconds t)
            in prim("sml_poll", (getCtx(),pds,tm_out,Poll))
               handle Poll => raise SysErr ("poll error",NONE)
            end

        (* check for conditions *)
        fun isPri (pi:poll_info) = #pri pi
        fun isIn (pi:poll_info) = #rd pi
        fun isOut (pi:poll_info) = #wr pi
        fun infoToPollDesc x = x
      end
  end
