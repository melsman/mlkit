
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
er1.sml               ccl ecte
er2.sml               ccl ecte
er3.sml               ccl ecte
er4.sml               ccl ecte
er5.sml               ccl ecte
er6.sml               ccl ecte
er7.sml               ccl ecte
er8.sml               ccl ecte
er9.sml               ccl ecte
er10.sml              ccl ecte
er11.sml              ccl ecte
er12.sml              ccl ecte
er13.sml              ccl ecte
param.sml
tup.sml
rec.sml
call.sml
call2.sml