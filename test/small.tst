
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

(* Tests of some benchmark programs *)

kitfib35.sml            tx            nobasislib
kitdangle.sml           tx            nobasislib
kitdangle3.sml          tx            nobasislib
kitreynolds2.sml        tx
kitreynolds3.sml        tx
kitloop2.sml            tx   
kittmergesort.sml       tx tc
kitqsort.sml            tx tc
kitmandelbrot.sml       tx tc
kitlife35u.sml          tx tc
klife_eq.sml            tx tc
kitkbjul9.sml           tx tc
kkb_eq.sml              tx tc
kkb36c.sml              tx tc
kitsimple.sml           tx tc
fft.sml                 tx tc
msort.pm                tx tc
tststrcmp.sml                
FuhMishra.pm            tx tc
