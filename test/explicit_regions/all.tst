(*  TEST FILE

A test file is a file that mentions a set of Standard ML sources and
projects. A test file has extension `.tst'.  Entries in a test file
consists of a file name path (with extension sml, sig, or mlb) followed
by a list of tokens. The following tokens are supported:

 nobasislib     ; do not import basis library
 noopt          ; disable lambda optimiser
 ccl            ; compare compiler logs
 tx             ; time executable
 tc             ; time compiler
 ecte           ; expect compile time error
 ue             ; expect uncaught exception

Test files may contain Standard ML like comments.

*)

(* ------------------------------ *)
(* Testing expression annotations *)
(* ------------------------------ *)

er1.sml               ccl ecte nobasislib noopt  (* It is an error to declare a region with a name that is already in scope (region decs) *)
er2.sml               ccl ecte nobasislib noopt  (* It is an error to have duplicate region names in a region declaration *)
er3.sml               ccl ecte nobasislib noopt  (* A region cannot be used for values that belong to different region types *)
er4.sml               ccl ecte nobasislib noopt  (* It is an error to refer to a region that is not in scope *)
er5.sml               ccl ecte nobasislib noopt  (* It is an error to declare a region variable that is already in scope (function params) *)
er6.sml               ccl ecte nobasislib noopt  (* It is an error to unify a region parameter with a global region variable *)
er7.sml               ccl ecte nobasislib noopt  (* It is an error to unify a local region with a global region variable *)
er8.sml               ccl ecte nobasislib noopt  (* It is an error to unify a local region with a region parameter *)
er9.sml               ccl ecte nobasislib noopt  (* It is an error to unify two region parameters *)
er10.sml              ccl ecte nobasislib noopt  (* It is an error for a value in an explicit local region to escape *)
er11.sml              ccl ecte nobasislib noopt  (* It is an error for a value in an explicit local region to escape in a closure *)
er12.sml              ccl ecte nobasislib noopt  (* It is an error to refer to a region that is not in scope (function parameter) *)
er13.sml              ccl ecte nobasislib noopt  (* It is an error to call a function with a different non-zero number of region parameters than the function declares *)
er14.sml              ccl ecte nobasislib noopt  (* It is an error to give multiple explicit regions to ref *)
er15.sml              ccl ecte nobasislib noopt  (* It is an error to give multiple explicit regions to con0 *)
er16.sml              ccl ecte nobasislib noopt  (* It is an error to give multiple explicit regions to con1 *)
er17.sml              ccl ecte nobasislib noopt  (* It is an error to have constructors of different conditional branches to be allocated in different regions *)
er18.sml              ccl ecte nobasislib noopt  (* Empty record types cannot be annotated with explicit regions *)
er19.sml              ccl ecte nobasislib noopt  (* It is an error to use an effect variable for allocation *)
param.sml                      nobasislib noopt  (* Functions can be declared to take region parameters *)
param1.sml                     nobasislib noopt  (* Functions can be declared to take one region parameter *)
tup.sml                        nobasislib noopt  (* Tuples can be allocated in explicit regions *)
tup2.sml                       nobasislib noopt  (* Two tuples can be allocated in the same explicit region *)
rec.sml                        nobasislib noopt  (* Records can be allocated in explicit regions *)
string.sml                     nobasislib noopt  (* Strings can be allocated in explicit regions *)
ref.sml                        nobasislib noopt  (* Ref constructor can be allocated in explicit regions *)
con0.sml                       nobasislib noopt  (* Con0 constructor can be allocated in explicit regions *)
con1.sml                       nobasislib noopt  (* Con1 constructor can be allocated in explicit regions *)
call.sml                       nobasislib noopt
call2.sml                      nobasislib noopt
ty1.sml                        nobasislib noopt  (* Types can be annotated with explicit regions *)

(* ----------------------------------- *)
(* Testing the use of type annotations *)
(* ----------------------------------- *)

err_expty1.sml        ccl ecte nobasislib noopt  (* The global region variable of type T cannot be associated with a pair type *)
err_expty2.sml        ccl ecte nobasislib noopt  (* The global region variable of type pair cannot be associated with a function type *)
err_expty3.sml        ccl ecte nobasislib noopt  (* Escaping functions cannot live in local regions *)
err_patty1.sml        ccl ecte nobasislib noopt  (* The global region variable of type T cannot be associated with a pair type (pattern) *)
err_funty1.sml        ccl ecte nobasislib noopt  (* The global region variable of type T cannot be associated with a pair type (function return type) *)
err_funty2.sml        ccl ecte nobasislib noopt  (* Inconsistent use of a parameter region. A region cannot hold both pairs and triples. *)
err_funty3.sml        ccl ecte nobasislib noopt  (* Inconsistent use of a parameter region. A region cannot hold both pairs and strings. *)

expty1.sml                     nobasislib noopt  (* A local function can be forced into a global region *)
expty2.sml                     nobasislib noopt  (* A locally generated function that is returned can be stored in a passed region *)

err_ty1.sml           ccl ecte nobasislib noopt  (* An effect variable (beginning with 'e') cannot be used as a region variable *)
funty1.sml                     nobasislib noopt  (* A local function can be forced to have toplevel effect *)
err_copylist.sml      ccl ecte nobasislib noopt  (* Exomorphisms by non-unifiable explicit region variables *)

effty1.sml            ccl ecte nobasislib noopt  (* A function with a local effect cannot escape *)

nomut-err.sml         ccl ecte nobasislib noopt  (* Violation of nomut constraint on function *)
nomut-ok.sml                   nobasislib noopt  (* Satisfiability of nomut constraint on function *)

nomut2-err.sml        ccl ecte nobasislib noopt  (* Violation of nomut constraint on function instantiation *)

disputs.sml           ccl ecte nobasislib noopt  (* Violation of disjoint put-effects (##) - let rec *)
disputs2.sml          ccl ecte nobasislib noopt  (* Violation of disjoint put-effects (##) - fun *)

par.sml                        nobasislib noopt  (* A sound implementation of par with constraint *)
par-no.sml            ccl ecte nobasislib noopt  (* But it needs to be satisfied *)
par-no2.sml           ccl ecte nobasislib noopt  (* The trivial definition of par is not ok *)

mod.sml               ccl ecte                   (* Signature matching: It is an error if the implementation
                                                    type is less general than the specified type. *)
mod2.sml              ccl ecte                   (* Signature matching: It is an error if the specified
                                                    type is less general than the implementation type. *)
mod3.sml              ccl ecte                   (* Signature matching: It is an error if the implementation type
                                                    is less general than the specified type. *)
mod4.sml              ccl ecte                   (* Signature matching: It is an error if the implementation type
                                                    is less general than the specified type (enrichment). *)
mod5.sml                                         (* Signature matching: ok. *)
mod6.sml              ccl ecte                   (* Signature matching: It is an error if the specification type
                                                    is less general than the implementation type. *)
mod7.sml              ccl ecte                   (* Signature matching: It is an error if the specification type
                                                    is less general than the implementation type. *)