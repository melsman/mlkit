
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
