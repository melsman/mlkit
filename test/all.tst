
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

(* Tests of static semantics; it would be great with some more
 * systematic testing here! *)

valrecpat.sml
valrecpat1.sml
valrecpat2.sml
valrecpat3.sml
valrecpat4.sml
posix.sml
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
danwang.mlb
testmatc.sml        ccl
excon.sml
dangle4.sml                   
ftv.sml
elabDecBug.sml      ccl ecte
oh-no.sml           ccl       nobasislib
oh-no2.sml          ccl       nobasislib

(* Tests of some benchmark programs *)

kitfib35.sml            tx    nobasislib
kitdangle.sml           tx    nobasislib
kitdangle3.sml          tx    nobasislib
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
vliw.sml                tx tc 
lexgen.sml              tx tc 
mlyacc.mlb              tx tc 
logic.mlb               tx tc 
barnes-hut.mlb          tx tc 
nucleic.mlb             tx tc 
ray.mlb                 tx tc 
ratio-regions.sml       tx tc 
kitmolgard.sml          tx tc 
msort.mlb               tx tc 
tststrcmp.sml                 
FuhMishra.mlb           tx tc 

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc 
check_arrays.sml              
array.sml
arrayslice.sml          
array2.sml
general.sml                   
int.sml                       
int_2.sml                     
int31.sml                     
int31_2.sml                   
int32.sml                     
int32_2.sml                   
intinf.sml
list.sml                      
listpair.sml                  
string.sml                    
stringcvt.sml                 
textio.sml                    
vector.sml
vectorslice.sml                
word8vector.sml
word8vectorslice.sml           
word8array.sml                
word8arrayslice.sml
substring.sml
bytechar.sml                  
time.sml                      
math.sml                                       (* ok, but not quite the 
                                                  semantics of the basis 
                                                  library specification *)
listsort.mlb                  
date.sml                      
timer.sml                     
unixpath.sml                  
cmdline.sml                   
filesys.sml                                    (* See test/README *)
real.sml                      
word.sml                      
word8.sml                     
word31.sml                    
word32.sml                    
regexp1.mlb                   
regexp2.mlb                   
pickletest.mlb                    
packreal.sml                  
patricia.sml                  
stream.mlb                    
natset.sml                    
fns.sml                       
datatypes.sml                 
