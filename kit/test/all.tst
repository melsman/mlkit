
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
 gengc          ; also activate generational collection
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
unbox.sml
rank.sml            ccl       ecte    nobasislib
pat.sml             ccl     
llv.sml
weeks.sml
weeks2.sml          ccl       ecte
weeks3.sml          ccl       ecte
weeks5.sml
weeks6.sml                    ecte
typerr.sml          ccl       ecte
testcon.sml         ccl       ecte
freedatatype.sml    ccl       ecte
freedatatype2.sml   ccl       ecte
sharing.sml         ccl       ecte
opaque.sml          ccl
opaque2.sml                        ue nobasislib
opaque3.sml                        nobasislib
functor.sml         ccl
functor2.sml        ccl
functor3.sml
constraint.sml
lex.sml                               nobasislib
layout.sml
anoq_Exception.sml
danwang.pm                    tc
testmatc.sml        ccl
excon.sml
dangle4.sml                   gc gengc prof
ftv.sml
elabDecBug.sml      ccl ecte
oh-no.sml           ccl       gc nobasislib
oh-no2.sml          ccl       gc nobasislib

(* Tests of some benchmark programs *)

kitfib35.sml            tx    gc gengc prof   nobasislib
kitdangle.sml           tx             prof   nobasislib
kitdangle3.sml          tx             prof   nobasislib
kitreynolds2.sml        tx    gc gengc prof
kitreynolds3.sml        tx    gc gengc prof
kitloop2.sml            tx    gc gengc prof
kittmergesort.sml       tx tc gc gengc prof
kitqsort.sml            tx tc gc gengc prof
kitmandelbrot.sml       tx tc gc gengc prof
kitlife35u.sml          tx tc gc gengc prof
klife_eq.sml            tx tc gc gengc prof
kitkbjul9.sml           tx tc gc gengc prof
kkb_eq.sml              tx tc gc gengc prof
kkb36c.sml              tx tc gc gengc prof
kitsimple.sml           tx tc gc gengc prof
fft.sml                 tx tc gc gengc prof
vliw.sml                tx tc gc gengc prof
lexgen.sml              tx tc gc gengc prof
mlyacc.pm               tx tc gc gengc prof
logic.pm                tx tc gc gengc prof
barnes-hut.pm           tx tc gc gengc prof
nucleic.pm              tx tc gc gengc prof
ray.pm                  tx tc gc gengc prof
ratio-regions.sml       tx tc gc gengc prof
kitmolgard.sml          tx tc gc gengc prof
msort.pm                tx tc gc gengc prof
tststrcmp.sml                 gc gengc prof
FuhMishra.pm            tx tc gc gengc prof

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc gc gengc prof
check_arrays.sml              gc gengc prof
array.sml                     gc gengc prof
array2.sml                    gc gengc prof
general.sml                   gc gengc prof
int.sml                       gc gengc prof
int_2.sml                     gc gengc prof
int31.sml                     gc gengc prof
int31_2.sml                   gc gengc prof 
int32.sml                     gc gengc prof
int32_2.sml                   gc gengc prof
list.sml                      gc gengc prof
listpair.sml                  gc gengc prof
string.sml                    gc gengc prof
stringcvt.sml                 gc gengc prof
textio.sml                    gc gengc prof
vector.sml                    gc gengc prof
word8vector.sml               gc gengc prof
word8array.sml                gc gengc prof
bytechar.sml                  gc gengc prof
time.sml                      gc gengc prof
math.sml                      gc gengc prof    (* ok, but not quite the 
                                                  semantics of the basis 
                                                  library specification *)
listsort.sml                  gc gengc prof
date.sml                      gc gengc prof
timer.sml                     gc gengc prof
unixpath.sml                  gc gengc prof
cmdline.sml                   gc gengc prof
filesys.sml                   gc gengc prof    (* See test/README *)
real.sml                      gc gengc prof
word.sml                      gc gengc prof
word8.sml                     gc gengc prof
word31.sml                    gc gengc prof
word32.sml                    gc gengc prof
regexp1.sml                   gc gengc prof
regexp2.sml                   gc gengc prof
pickle.pm                     gc gengc prof
