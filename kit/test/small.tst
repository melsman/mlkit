
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
(*kittmergesort.sml       tx 
kitlife35u.sml          tx 
lexgen.sml              tx 
professor2.sml          tx *)
kkb36c.sml              tx 
vliw.sml                tx 
logic.pm
(*
mlyacc.pm               tx 
barnes-hut.pm           tx 
nucleic.pm              tx 
ray.pm                  tx 
ratio-regions.sml       tx 
*)