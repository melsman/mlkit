
(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or pm) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 prof           ; also compile and compare runtime output with 
                  profiling enabled

Test files may contain Standard ML like comments.

*)

kittmergesort.sml       tx tc gc
kitqsort.sml            tx tc gc
kitmandelbrot.sml       tx tc gc
kitsimple.sml           tx tc gc
kitlife35u.sml          tx tc gc
kkb36c.sml              tx tc gc
fft.sml                 tx tc gc
(*
vliw.sml                tx tc gc
lexgen.sml              tx tc gc
mlyacc.pm               tx tc gc
logic.pm                tx tc gc
barnes-hut.pm           tx tc gc
nucleic.pm              tx tc gc
ray.pm                  tx tc gc
ratio-regions.sml       tx tc gc
*)