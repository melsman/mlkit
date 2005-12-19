(* This structure declares values that must be initialised when the
 * system starts executing. The purpose is to allow the clients of
 * this structure to be discharged at link time; only files that are
 * safe (no side effects) can be discharged at link time. ME 1998-08-21 *)

structure Initial =
  struct
    infix - + *

    type int0 = int        
    type word0 = word         (* used by WORD signature *)

    exception Fail of string
    val _ = prim("sml_setFailNumber", (Fail "hat" : exn, 1 : int)) : unit;

    (* Time structure *)
    val timebase : int = prim("get_time_base", 0)
(*    val timebase = ~1073741820 - 4 13/04/1999, Niels*)

    (* Date structure *)
    local fun localoffset_ () : real = prim("sml_localoffset", ())
    in val localoffset = localoffset_ ()
       val fail_asctime = Fail "asctime"
       val fail_strftime = Fail "strftime"
    end

    (* Timer *)
    local
      type tusage = {gcSec : int,  gcUsec : int,
		     sysSec : int, sysUsec : int,
		     usrSec : int, usrUsec : int}
      fun getrealtime_ () : {sec : int, usec : int} =
	prim("sml_getrealtime", ())
      fun getrutime_ () : tusage = prim("sml_getrutime", ())
    in val initial_realtime = getrealtime_ ()
       val initial_rutime = getrutime_ ()
    end 

    (* Real structure *)
    local
      fun get_posInf () : real = prim ("posInfFloat", ())
      fun get_negInf () : real = prim ("negInfFloat", ())
    in
      val posInf = get_posInf()
      val negInf = get_negInf()
    end

    (* Math structure *)
    local
      fun sqrt (r : real) : real = prim ("sqrtFloat", r)
      fun ln' (r : real) : real = prim ("lnFloat", r)
    in
      val ln10 = ln' 10.0 
      val NaN = sqrt ~1.0
    end

    (* Int structure. Integers are untagged (or tagged if GC is enabled), 
     * and there is a limit to the size of immediate integers that the Kit 
     * accepts. We should change the lexer such that it does not convert a 
     * string representation of an integer constant into an internal 
     * integer, as this makes the the kit dependent on the precision of 
     * the compiler (SML/NJ) that we use to compile the Kit. *)

    type int0 = int
    val maxInt0 : int = prim("max_fixed_int", 0)
    val minInt0 : int = prim("min_fixed_int", 0)
    val precisionInt0 : int = prim("precision", 0)

    (* TextIO *)
    val stdIn_stream : int = prim ("stdInStream", 0)
    val stdOut_stream : int = prim ("stdOutStream", 0)
    val stdErr_stream : int = prim ("stdErrStream", 0)
    val failscan : exn = Fail "scanStream: backtracking too far"

    (* FileSys *)
    structure FileSys =
      struct
        val filesys_fail : exn = Fail "FileSys"    
      end

    (* Process *)
    val exittasks = (ref []) : (unit -> unit) list ref
  

    (* Posix *)

     structure TextIO =
       struct
         val bufsize = 4000
       end


     structure Posix_Values =
       struct
         fun getN s = prim("@sml_syserror", s : string) : int
         fun getNS s = prim("@sml_findsignal", s : string) : int
         structure Err =
           struct
             val acces = getN "EACCES"
             val again = getN "EAGAIN"
             val badf = getN "EBADF"
             val badmsg = getN "EBADMSG"
             val busy = getN "EBUSY"
             val canceled = getN "ECANCELED"
             val child = getN "ECHILD"
             val deadlk = getN "EDEADLK"
             val dom = getN "EDOM"
             val exist = getN "EEXIST"
             val fault = getN "EFAULT"
             val fbig = getN "EFBIG"
             val inprogress = getN "EINPROGRESS"
             val intr = getN "EINTR"
             val inval = getN "EINVAL"
             val io = getN "EIO"
             val isdir = getN "EISDIR"
             val loop = getN "ELOOP"
             val mfile = getN "EMFILE"
             val mlink = getN "EMLINK"
             val msgsize = getN "EMSGSIZE"
             val nametoolong = getN "ENAMETOOLONG"
             val nfile = getN "ENFILE"
             val nodev = getN "ENODEV"
             val noent = getN "ENOENT"
             val noexec = getN "ENOEXEC"
             val nolck = getN "ENOLCK"
             val nomem = getN "ENOMEM"
             val nospc = getN "ENOSPC"
             val nosys = getN "ENOSYS"
             val notdir = getN "ENOTDIR"
             val notsup = getN "ENOTSUP"
             val notsock = getN "ENOTSOCK"
             val notempty = getN "ENOTEMPTY"
             val notty = getN "ENOTTY"
             val nxio = getN "ENXIO"
             val perm = getN "EPERM"
             val pipe = getN "EPIPE"
             val range = getN "ERANGE"
             val rofs = getN "EROFS"
             val spipe = getN "ESPIPE"
             val srch = getN "ESRCH"
             val toobig = getN "E2BIG"
             val xdev = getN "EXDEV"
           end
         structure Signal =
           struct
             val abrt = getNS "SIGABRT"
             val alrm = getNS "SIGALRM"
             val bus  = getNS "SIGBUS"
             val fpe  = getNS "SIGFPE"
             val hup  = getNS "SIGHUP"
             val ill  = getNS "SIGILL"
             val int  = getNS "SIGINT"
             val kill = getNS "SIGKILL"
             val pipe = getNS "SIGPIPE"
             val quit = getNS "SIGQUIT"
             val segv = getNS "SIGSEGV"
             val term = getNS "SIGTERM"
             val usr1 = getNS "SIGUSR1"
             val usr2 = getNS "SIGUSR2"
             val chld = getNS "SIGCHLD"
             val cont = getNS "SIGCONT"
             val stop = getNS "SIGSTOP"
             val tstp = getNS "SIGTSTP"
             val ttin = getNS "SIGTTIN"
             val ttou = getNS "SIGTTOU"
           end
       end

      structure Posix_File_Sys =
        struct
        val (stdin,stdout,stderr) = prim ("sml_getStdNumbers", ()) : (int * int * int)
        structure O = 
          struct
            val append   =  0wx1
            val excl     =  0wx2
            val noctty   =  0wx4
            val nonblock =  0wx8
            val sync     = 0wx10
            val trunc    = 0wx20
            val text     = 0wx40
            val bin      = 0wx80
            val rdonly   = 0wx100
            val wronly   = 0wx200
            val rdwr     = 0wx400
          end

        structure S =
          struct
            val irwxu =    0wx1
            val irusr =    0wx2
            val iwusr =    0wx4
            val ixusr =    0wx8
            val irwxg =   0wx10
            val irgrp =   0wx20
            val iwgrp =   0wx40
            val ixgrp =   0wx80
            val irwxo =  0wx100
            val iroth =  0wx200
            val iwoth =  0wx400
            val ixoth =  0wx800
            val isuid = 0wx1000
            val isgid = 0wx2000
          end
        end
 


  end
