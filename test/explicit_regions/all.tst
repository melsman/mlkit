
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
er1.sml               ccl ecte    (* It is an error to declare a region with a name that is already in scope (region decs) *)
er2.sml               ccl ecte    (* It is an error to have duplicate region names in a region declaration *)
er3.sml               ccl ecte    (* A region cannot be used for values that belong to different region types *)
er4.sml               ccl ecte    (* It is an error to refer to a region that is not in scope *)
er5.sml               ccl ecte    (* It is an error to declare a region variable that is already in scope (function params) *)
er6.sml               ccl ecte    (* It is an error to unify a region parameter with a global region variable *)
er7.sml               ccl ecte    (* It is an error to unify a local region with a global region variable *)
er8.sml               ccl ecte    (* It is an error to unify a local region with a region parameter *)
er9.sml               ccl ecte    (* It is an error to unify two region parameters *)
er10.sml              ccl ecte    (* It is an error for a value in an explicit local region to escape *)
er11.sml              ccl ecte    (* It is an error for a value in an explicit local region to escape in a closure *)
er12.sml              ccl ecte    (* It is an error to refer to a region that is not in scope (function parameter) *)
er13.sml              ccl ecte    (* It is an error to call a function with a different non-zero number of region parameters than the function declares *)
er14.sml              ccl ecte    (* It is an error to give multiple explicit regions to ref *)

param.sml                         (* Functions can be declared to take region parameters *)
param1.sml                        (* Functions can be declared to take one region parameter *)
tup.sml                           (* Tuples can be allocated in explicit regions *)
tup2.sml                          (* Two tuples can be allocated in the same explicit region *)
rec.sml                           (* Records can be allocated in explicit regions *)
string.sml                        (* Strings can be allocated in explicit regions *)
ref.sml                           (* Ref cells can be allocated in explicit regions *)
call.sml
call2.sml