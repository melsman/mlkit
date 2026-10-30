(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
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


(* General -- incomplete 1996-04-19, 1996-09-30, 1997-03-12 *)


val _ = print "\nFile general.sml: Testing structure General...\n"

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

exception E1;
exception E2 = E1;

(* Barry gives local exceptions unique generated names. Check aliases without
 * making the expected output depend on the compiler's numeric name keys. *)
fun stableName s = if s = exnName E1 then "E1" else s
fun prExn(exnStr, exn) =
    (print "\nShould be `"; print exnStr; print "':\n  ";
     print (stableName (exnName exn)); print "\n  ";
     print (stableName (exnMessage exn)));

val _ = List.map prExn
    [("E1 or E2",  E2),
     ("Bind",      getExn(fn _ => let val true = false in () end)),
     ("Match",     getExn(fn _ => (fn true => ()) false)),
     ("Subscript", getExn(fn _ => Vector.sub(vector [], ~1))),
     ("Size",      getExn(fn _ => Array.array(Array.maxLen+1, ()))),
(*   ("Overflow",  getExn(fn _ => Math.exp 1E99)),
     ("Domain",    getExn(fn _ => Math.ln ~1.0)),
*)   ("Div",       getExn(fn _ => 1 div 0)),
     ("Chr",       getExn(fn _ => Char.chr 9999999)),
     ("Fail",      Fail "demo"),
     ("Option",    getExn(fn _ => valOf NONE)),
     ("Empty",     getExn(fn _ => List.hd [])),
(*     ("SysErr",    getExn (fn _ => FileSys.modTime "exists.not")), *)
     ("Io",        getExn (fn _ => TextIO.openOut "."))];
