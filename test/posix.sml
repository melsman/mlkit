
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p =
    let open Int
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds


val _ = print "\nFile posix.sml: Testing structure Posix...\n"
val _ = print "\nFile posix.sml: Testing structure Posix.IO...\n"

val channels = ref NONE
val _ = tst' "Posix.IO.pipe" (fn () => let val {outfd,infd} = Posix.IO.pipe ()
                                       in (channels := (SOME(infd,outfd)) ; true)
                                       end)

val (infd,outfd) = Option.valOf (!channels)

val () = tst0 "Posix.Process.fork - start" "OK"

val () =
    case Posix.Process.fork () of
        NONE =>
        ( tst0 "Posix.Process.fork - in child" "OK"
        ; tst' "Posix.IO.close" (fn () => (Posix.IO.close outfd; true))
	; Posix.Process.exit(0w0)
        )
      | SOME pid =>
        ( Posix.IO.close infd
	; Posix.Process.wait()
        ; tst0 "Posix.Process.fork - in parent - child finished" "OK"
        )

fun lookup s a =
    case List.find (fn (f,_) => f = s) a of
	SOME (_, name) => SOME name
      | NONE => NONE

val _ = tst' "Posix.uname" (fn () =>
			       let val a = Posix.ProcEnv.uname()
			       in case lookup "sysname" a of
				      SOME s => s = "Linux" orelse s = "Darwin" orelse s = "FreeBSD"
				    | NONE => false
			       end)

val _ = tst' "Posix.FileSys.fstat" (fn () =>
  let
    open Posix.FileSys
    val file = openf ("posix.sml", O_RDONLY, O.fromWord 0w0)
    val size1 = Position.toInt (ST.size (stat "posix.sml"))
    val size2 = Position.toInt (ST.size (fstat file))
  in
    size1 = size2
  end)

val _ = print "\nFile posix.sml: Testing structure Posix.TTY...\n"

val _ = tst "Posix.TTY.V.nccs > 0" (Posix.TTY.V.nccs > 0)

val _ = tst' "Posix.TTY.V.cc/update/sub" (fn () =>
  let
    val cs = Posix.TTY.V.cc [(Posix.TTY.V.eof, #"\^D")]
    val cs' = Posix.TTY.V.update (cs, [(Posix.TTY.V.eol, #"\n")])
  in
    Posix.TTY.V.sub (cs', Posix.TTY.V.eof) = #"\^D" andalso
    Posix.TTY.V.sub (cs', Posix.TTY.V.eol) = #"\n"
  end)

val _ = tst' "Posix.TTY.CF" (fn () =>
  let
    val t = Posix.TTY.termios { iflag = Posix.TTY.I.flags [],
                                oflag = Posix.TTY.O.flags [],
                                cflag = Posix.TTY.C.flags [],
                                lflag = Posix.TTY.L.flags [],
                                cc = Posix.TTY.V.cc [],
                                ispeed = Posix.TTY.b9600,
                                ospeed = Posix.TTY.b9600
                              }
    val t' = Posix.TTY.CF.setispeed (Posix.TTY.CF.setospeed (t, Posix.TTY.b1200),
                                     Posix.TTY.b2400)
  in
    Posix.TTY.CF.getispeed t' = Posix.TTY.b2400 andalso
    Posix.TTY.CF.getospeed t' = Posix.TTY.b1200
  end)

val _ = tst' "Posix.TTY.TC.getattr" (fn () =>
  if Posix.ProcEnv.isatty Posix.FileSys.stdin
  then
    let
      val t = Posix.TTY.TC.getattr Posix.FileSys.stdin
      val _ = Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, t)
      val t' = Posix.TTY.TC.getattr Posix.FileSys.stdin
      val cc = Posix.TTY.getcc t
      val cc' = Posix.TTY.getcc t'
    in
      Posix.TTY.getiflag t = Posix.TTY.getiflag t' andalso
      Posix.TTY.getoflag t = Posix.TTY.getoflag t' andalso
      Posix.TTY.getcflag t = Posix.TTY.getcflag t' andalso
      Posix.TTY.getlflag t = Posix.TTY.getlflag t' andalso
      Posix.TTY.CF.getispeed t = Posix.TTY.CF.getispeed t' andalso
      Posix.TTY.CF.getospeed t = Posix.TTY.CF.getospeed t' andalso
      Posix.TTY.V.sub(cc, Posix.TTY.V.eof) = Posix.TTY.V.sub(cc', Posix.TTY.V.eof) andalso
      Posix.TTY.V.sub(cc, Posix.TTY.V.min) = Posix.TTY.V.sub(cc', Posix.TTY.V.min) andalso
      Posix.TTY.V.sub(cc, Posix.TTY.V.time) = Posix.TTY.V.sub(cc', Posix.TTY.V.time)
    end
  else
    ((Posix.TTY.TC.getattr Posix.FileSys.stdin; false)
     handle OS.SysErr (_, SOME e) => e = Posix.Error.notty
          | OS.SysErr _ => true))

val _ = tst' "Posix.TTY.TC.setattr cc roundtrip" (fn () =>
  if Posix.ProcEnv.isatty Posix.FileSys.stdin
  then
    let
      val t = Posix.TTY.TC.getattr Posix.FileSys.stdin
      val {iflag,oflag,cflag,lflag,cc,ispeed,ospeed} = Posix.TTY.fieldsOf t
      val old = Posix.TTY.V.sub (cc, Posix.TTY.V.eof)
      val new = if old = #"\^D" then #"\^E" else #"\^D"
      fun restore () = Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, t)
      val got =
          ((let
              val t' = Posix.TTY.termios { iflag = iflag, oflag = oflag,
                                           cflag = cflag, lflag = lflag,
                                           cc = Posix.TTY.V.update (cc, [(Posix.TTY.V.eof, new)]),
                                           ispeed = ispeed, ospeed = ospeed
                                         }
              val _ = Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, t')
            in
              Posix.TTY.V.sub (Posix.TTY.getcc (Posix.TTY.TC.getattr Posix.FileSys.stdin),
                               Posix.TTY.V.eof)
            end)
           handle e => (restore (); raise e))
      val _ = restore ()
    in
      got = new
    end
  else true)
(*
val _ = tst' "Posix.TTY.TC.setattr invalid speed" (fn () =>
  let
    val bad = Posix.TTY.wordToSpeed (SysWord.notb 0w0)
    val t = Posix.TTY.termios { iflag = Posix.TTY.I.flags [],
                                oflag = Posix.TTY.O.flags [],
                                cflag = Posix.TTY.C.flags [],
                                lflag = Posix.TTY.L.flags [],
                                cc = Posix.TTY.V.cc [],
                                ispeed = bad,
                                ospeed = bad
                              }
  in
    (Posix.TTY.TC.setattr (Posix.FileSys.stdin, Posix.TTY.TC.sanow, t); false)
    handle OS.SysErr (_, SOME e) => e = Posix.Error.inval
         | OS.SysErr _ => false
  end)
*)
fun expectSyserr f = (f (); false) handle OS.SysErr _ => true

val fderr = Posix.FileSys.wordToFD(SysWord.fromInt ~1)

val _ = tst "Posix.TTY.TC.sendbreak invalid fd"
            (expectSyserr (fn () => Posix.TTY.TC.sendbreak (fderr, 0)))
val _ = tst "Posix.TTY.TC.drain invalid fd"
            (expectSyserr (fn () => Posix.TTY.TC.drain fderr))
val _ = tst "Posix.TTY.TC.flush invalid fd"
            (expectSyserr (fn () => Posix.TTY.TC.flush (fderr, Posix.TTY.TC.iflush)))
val _ = tst "Posix.TTY.TC.flow invalid fd"
            (expectSyserr (fn () => Posix.TTY.TC.flow (fderr, Posix.TTY.TC.ion)))
val _ = tst "Posix.TTY.TC.getpgrp invalid fd"
            (expectSyserr (fn () => Posix.TTY.TC.getpgrp fderr))
val _ = tst "Posix.TTY.TC.setpgrp invalid fd"
            (expectSyserr (fn () =>
                             Posix.TTY.TC.setpgrp (fderr, Posix.Process.wordToPid 0w1)))
