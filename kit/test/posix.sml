
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

val chanels = ref NONE

val _ = tst' "Posix.IO.pipe" (fn () => let val {outfd,infd} = Posix.IO.pipe () 
                                       in (chanels := (SOME(infd,outfd)) ; true)
                                       end)

val (infd,outfd) = Option.valOf (!chanels)

val child = (Posix.Process.fork () = NONE before tst0 "Posix.Process.fork" "OK") 
            handle _ => (tst0 "Posix.Process.fork" "EXN" before OS.Process.exit(OS.Process.failure);true)

val _ = if child then let
val _ = tst' "Posix.IO.close" (fn () => (Posix.IO.close outfd; true))
in () end
else let
val _ = tst' "Posix.IO.close" (fn () => (Posix.IO.close infd; true))
in () end
