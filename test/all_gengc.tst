
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
dangle4.sml                   gengc 
ftv.sml
elabDecBug.sml      ccl ecte
oh-no.sml           ccl       nobasislib
oh-no2.sml          ccl       nobasislib

(* Tests of some benchmark programs *)

kitfib35.sml            tx    gengc    nobasislib
kitdangle.sml           tx                nobasislib
kitdangle3.sml          tx                nobasislib
kitreynolds2.sml        tx    gengc 
kitreynolds3.sml        tx    gengc 
kitloop2.sml            tx    gengc 
kittmergesort.sml       tx tc gengc 
kitqsort.sml            tx tc gengc 
kitmandelbrot.sml       tx tc gengc 
kitlife35u.sml          tx tc gengc 
klife_eq.sml            tx tc gengc 
kitkbjul9.sml           tx tc gengc 
kkb_eq.sml              tx tc gengc 
kkb36c.sml              tx tc gengc 
kitsimple.sml           tx tc gengc 
fft.sml                 tx tc gengc 
vliw.sml                tx tc gengc 
lexgen.sml              tx tc gengc 
mlyacc.pm               tx tc gengc 
logic.pm                tx tc gengc 
barnes-hut.pm           tx tc gengc 
nucleic.pm              tx tc gengc 
ray.pm                  tx tc gengc 
ratio-regions.sml       tx tc gengc 
kitmolgard.sml          tx tc gengc 
msort.pm                tx tc gengc 
tststrcmp.sml                 gengc 
FuhMishra.pm            tx tc gengc 

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc gengc 
check_arrays.sml              gengc 
array.sml                     gengc 
array2.sml                    gengc 
general.sml                   gengc 
int.sml                       gengc 
int_2.sml                     gengc 
int31.sml                     gengc 
int31_2.sml                   gengc  
int32.sml                     gengc 
int32_2.sml                   gengc 
list.sml                      gengc 
listpair.sml                  gengc 
string.sml                    gengc 
stringcvt.sml                 gengc 
textio.sml                    gengc 
vector.sml                    gengc 
word8vector.sml               gengc 
word8array.sml                gengc 
bytechar.sml                  gengc 
time.sml                      gengc 
math.sml                      gengc     (* ok, but not quite the 
                                                  semantics of the basis 
                                                  library specification *)
listsort.sml                  gengc 
date.sml                      gengc 
timer.sml                     gengc 
unixpath.sml                  gengc 
cmdline.sml                   gengc 
filesys.sml                   gengc     (* See test/README *)
real.sml                      gengc 
word.sml                      gengc 
word8.sml                     gengc 
word31.sml                    gengc 
word32.sml                    gengc 
regexp1.sml                   gengc 
regexp2.sml                   gengc 
pickle.pm                     gengc 
packreal.sml                  gengc 
natset.sml                    gengc
fns.sml                       gengc
datatypes.sml                 gengc