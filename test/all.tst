
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

(* Tests of static semantics; it would be great with some more
 * systematic testing here! *)

rank.sml            ccl       ecte    nobasislib
pat.sml             ccl     
llv.sml
weeks.sml
weeks2.sml          ccl       ecte
weeks3.sml          ccl       ecte
typerr.sml          ccl       ecte
testcon.sml         ccl       ecte
freedatatype.sml    ccl       ecte
freedatatype2.sml   ccl       ecte
sharing.sml         ccl       ecte
opaque.sml          ccl
opaque2.sml                           nobasislib
functor.sml         ccl
functor2.sml        ccl
constraint.sml
lex.sml                               nobasislib
layout.sml
anoq_Exception.sml
danwang.pm                    tc
testmatc.sml        ccl

(* Tests of some benchmark programs *)

kitfib35.sml            tx            nobasislib
kitdangle.sml           tx            nobasislib
kitdangle3.sml          tx            nobasislib
kitreynolds2.sml        tx
kitreynolds3.sml        tx
kitloop2.sml            tx   
kittmergesort.sml       tx tc prof
kitqsort.sml            tx tc
kitmandelbrot.sml       tx tc prof
kitlife35u.sml          tx tc
klife_eq.sml            tx tc prof
kitkbjul9.sml           tx tc
kkb_eq.sml              tx tc
kkb36c.sml              tx tc prof
kitsimple.sml           tx tc prof
fft.sml                 tx tc prof
vliw.sml                tx tc prof
lexgen.sml              tx tc prof
mlyacc.pm               tx tc prof
logic.pm                tx tc prof
barnes-hut.pm           tx tc prof
nucleic.pm              tx tc prof
ray.pm                  tx tc prof
ratio-regions.sml       tx tc prof
kitmolgard.sml          tx tc prof
msort.pm                tx tc prof
tststrcmp.sml                 prof
FuhMishra.pm            tx tc prof

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc prof
check_arrays.sml              prof
array.sml                     prof
general.sml                   prof
int.sml                       prof
int31.sml                     prof
int31_2.sml                   prof 
int32.sml                     prof
int32_2.sml                   prof
list.sml                      prof
listpair.sml                  prof
string.sml                    prof
stringcvt.sml                 prof
textio.sml                    prof
vector.sml                    prof
word8vector.sml               prof
word8array.sml                prof
bytechar.sml                  prof
time.sml                      prof
math.sml                      prof    (* ok, but not quite the 
                                         semantics of the basis 
                                         library specification *)
listsort.sml                  prof
date.sml                      prof
timer.sml                     prof
unixpath.sml                  prof
cmdline.sml                   prof
filesys.sml                   prof    (* See test/README *)
real.sml                      prof
word.sml                      prof
word8.sml                     prof
word31.sml                    prof
word32.sml                    prof
