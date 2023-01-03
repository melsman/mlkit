
(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or mlb) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 nooptimiser    ; disable lambda optimiser
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 ue             ; expect uncaught exception

Test files may contain Standard ML like comments.

*)

(* simple tests *)
spawn.sml                 nobasislib
spawn_simple.sml          nobasislib
spawn_simple2.sml         nobasislib
spawn_simple3.sml         nobasislib
spawn_simple4.sml         nobasislib
spawn_simple5.sml         nobasislib
spawn_simple6.sml         nobasislib
spawn100.sml              nobasislib
spawn_lists.sml           nobasislib
cores.sml                 nobasislib
exn0.sml                  nobasislib
exn1.sml                  nobasislib
exn2.sml                  nobasislib