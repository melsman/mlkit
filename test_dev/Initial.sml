(* This structure declares values that must be initialised when the
 * system starts executing. The purpose is to allow the clients of
 * this structure to be discharged at link time; only files that are
 * safe (no side effects) can be discharged at link time. ME 1998-08-21 *)

structure Initial =
  struct
    infix - + *

    fun print (s:string) : unit = prim("printStringML", s)
    val () = print "Starting\n"

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

    val () = print "Time\n"

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

    val () = print "Timer\n"

    (* Real structure *)
    local
      fun get_posInf () : real = prim ("posInfFloat", ())
      fun get_negInf () : real = prim ("negInfFloat", ())
    in
      val posInf = get_posInf()
      val negInf = get_negInf()
    end

    val () = print "Real\n"

    (* Math structure *)
    local
      fun sqrt (r : real) : real = prim ("sqrtFloat", r)
      fun ln' (r : real) : real = prim ("lnFloat", r)
    in
      val ln10 = ln' 10.0
      val NaN = sqrt ~1.0
    end

    val () = print "Math\n"

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

    val () = print "TextIO\n"

    (* FileSys *)
    structure FileSys =
      struct
        val filesys_fail : exn = Fail "FileSys"
      end

    (* Process *)
    val exittasks = (ref []) : (unit -> unit) list ref
    val exitCalled = ref false
    exception RaisedInExit

    val clearnerAtExit = (ref []) : (unit -> unit) list ref
    val addedclearner = ref false
   exception ClosedStream

    (* Posix *)

     structure TextIO =
       struct
         val bufsize = 4000
         val flushStdOut = ref (fn x => x) : (unit -> unit) ref
       end

    val () = print "Posix\n"

     structure Posix_Values =
       struct
         fun getN s =  prim("@sml_syserror", s : string) : int
         fun getNS s =  prim("@sml_findsignal", s : string) : int
         fun getT i =   prim("@sml_getTty", i : int) : word
         fun getTi i = prim("@sml_getTty", i : int) : int

         val () = print " - get\n"

         structure Tty =
           struct
             structure V =
               struct
                 val eof = getTi 0
                 val eol = getTi 1
                 val erase = getTi 2
                 val intr = getTi 3
                 val kill = getTi 4
                 val min = getTi 5
                 val quit = getTi 6
                 val susp = getTi 7
                 val time = getTi 8
                 val start = getTi 9
                 val stop = getTi 10
                 val nccs = getTi 70

                 val () = print " - getTi\n"
               end
             structure I =
               struct
                 val brkint = getT 11
                 val icrnl = getT 12
                 val ignbrk =  getT 13
                 val igncr =  getT 14
                 val ignpar =  getT 15
                 val inlcr =  getT 16
                 val inpck =  getT 17
                 val istrip =  getT 18
                 val ixoff =  getT 19
                 val ixon =  getT 20
                 val parmrk =  getT 21
                 val all = getT 44
                 val () = print " - getT\n"
               end
             structure O =
               struct
                 val opost = getT 22
                 val all = opost
               end
             structure C =
               struct
                 val clocal = getT 23
                 val cread = getT 24
                 val cs5 = getT 25
                 val cs6 = getT 26
                 val cs7 = getT 27
                 val cs8 = getT 28
                 val csize = getT 29
                 val cstopb = getT 30
                 val hupcl = getT 31
                 val parenb = getT 32
                 val parodd = getT 33
                 val all = getT 45
                 val () = print " - getT\n"
               end
             structure L =
               struct
                 val echo = getT 34
                 val echoe = getT 35
                 val echok = getT 36
                 val echonl = getT 37
                 val icanon = getT 38
                 val iexten = getT 39
                 val isig = getT 40
                 val noflsh = getT 41
                 val tostop = getT 42
                 val all = getT 46
                 val () = print " - getT\n"
               end
             structure Speed =
               struct
                 val b0 = getT 48
                 val b50 = getT 49
                 val b75 = getT 50
                 val b110 = getT 51
                 val b134 = getT 52
                 val b150 = getT 53
                 val b200 = getT 54
                 val b300 = getT 55
                 val b600 = getT 56
                 val b1200 = getT 57
                 val b1800 = getT 58
                 val b2400 = getT 59
                 val b4800 = getT 60
                 val b9600 = getT 61
                 val b19200 = getT 62
                 val b38400 = getT 63
                 val b57600 = getT 64
                 val b115200 = getT 65
                 val b230400 = getT 66
                 val () = print " - getT\n"
               end
           end

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
             val () = print " - getN\n"
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
             val () = print " - getNS\n"
           end

         structure Process =
           struct
             val untraced = 0wx1
             val nohang = 0wx2
             val all = untraced (* untraced *)
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

            val all      = 0wx3F (* [append,excl,noctty,nonblock,sync,trunc] *)
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

            val all   = 0wx3FFF
          end
        end

      val () = print "Done\n"

  end
