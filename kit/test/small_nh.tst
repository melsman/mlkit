
(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or pm) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 nooptimiser    ; disable lambda optimiser
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 prof           ; also compile and compare runtime output with 
                  profiling enabled
 gc             ; also activate garbage collection
 ue             ; expect uncaught exception

Test files may contain Standard ML like comments.

*)

(* Tests of some benchmark programs *)

klife_eq.sml            tx tc gc prof gengc
kitkbjul9.sml           tx tc gc prof gengc
kitsimple.sml           tx tc gc prof gengc
fft.sml                 tx tc gc prof gengc
vliw.sml                tx tc gc prof gengc
logic.pm                tx tc gc prof gengc
kitmolgard.sml          tx tc gc prof gengc


