
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

(* Tests of static semantics; it would be great with some more
 * systematic testing here! *)

valrecpat.sml
valrecpat1.sml
valrecpat2.sml
valrecpat3.sml
valrecpat4.sml
rank.sml            ccl       ecte    nobasislib
pat.sml             ccl     
llv.sml
weeks.sml
weeks2.sml          ccl       ecte
weeks3.sml          ccl       ecte
weeks5.sml
typerr.sml          ccl       ecte
testcon.sml         ccl       ecte
freedatatype.sml    ccl       ecte
freedatatype2.sml   ccl       ecte
sharing.sml         ccl       ecte
opaque.sml          ccl
opaque2.sml                        ue nobasislib
functor.sml         ccl
functor2.sml        ccl
constraint.sml
lex.sml                               nobasislib
layout.sml
anoq_Exception.sml
danwang.pm                    tc
testmatc.sml        ccl
excon.sml
dangle4.sml                   gc prof

(* Tests of some benchmark programs *)

kitfib35.sml            tx    gc prof   nobasislib
kitdangle.sml           tx       prof   nobasislib
kitdangle3.sml          tx       prof   nobasislib
kitreynolds2.sml        tx    gc prof
kitreynolds3.sml        tx    gc prof
kitloop2.sml            tx    gc prof
kittmergesort.sml       tx tc gc prof
kitqsort.sml            tx tc gc prof
kitmandelbrot.sml       tx tc gc prof
kitlife35u.sml          tx tc gc prof
klife_eq.sml            tx tc gc prof
kitkbjul9.sml           tx tc gc prof
kkb_eq.sml              tx tc gc prof
kkb36c.sml              tx tc gc prof
kitsimple.sml           tx tc gc prof
fft.sml                 tx tc gc prof
vliw.sml                tx tc gc prof
lexgen.sml              tx tc gc prof
mlyacc.pm               tx tc gc prof
logic.pm                tx tc gc prof
barnes-hut.pm           tx tc gc prof
nucleic.pm              tx tc gc prof
ray.pm                  tx tc gc prof
ratio-regions.sml       tx tc gc prof
kitmolgard.sml          tx tc gc prof
msort.pm                tx tc gc prof
tststrcmp.sml                 gc prof
FuhMishra.pm            tx tc gc prof

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc gc prof
check_arrays.sml              gc prof
array.sml                     gc prof
array2.sml                    gc prof
general.sml                   gc prof
int.sml                       gc prof
int_2.sml                     gc prof
int31.sml                     gc prof
int31_2.sml                   gc prof 
int32.sml                     gc prof
int32_2.sml                   gc prof
list.sml                      gc prof
listpair.sml                  gc prof
string.sml                    gc prof
stringcvt.sml                 gc prof
textio.sml                    gc prof
vector.sml                    gc prof
word8vector.sml               gc prof
word8array.sml                gc prof
bytechar.sml                  gc prof
time.sml                      gc prof
math.sml                      gc prof    (* ok, but not quite the 
                                            semantics of the basis 
                                            library specification *)
listsort.sml                  gc prof
date.sml                      gc prof
timer.sml                     gc prof
unixpath.sml                  gc prof
cmdline.sml                   gc prof
filesys.sml                   gc prof    (* See test/README *)
real.sml                      gc prof
word.sml                      gc prof
word8.sml                     gc prof
word31.sml                    gc prof
word32.sml                    gc prof
regexp1.sml                   gc prof
regexp2.sml                   gc prof
