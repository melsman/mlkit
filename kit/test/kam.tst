
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
typerr.sml          ccl       ecte
opaque.sml          ccl
opaque2.sml                           nobasislib
functor.sml         ccl
constraint.sml
lex.sml                               nobasislib
layout.sml

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

(* Tests of dynamic semantics and the Basis Library *)

testdyn1.sml               tc 
check_arrays.sml              
array.sml                     
general.sml                   
int.sml                       
list.sml                      
listpair.sml                  
string.sml                    
stringcvt.sml                 
textio.sml                    
vector.sml                    
word8vector.sml               
word8array.sml                
bytechar.sml                  
time.sml                      
math.sml                          (* ok, but not quite the 
                                     semantics of the basis 
                                     library specification *)
listsort.sml                  
date.sml                      
timer.sml                     
unixpath.sml                  
cmdline.sml                   
filesys.sml                       (* See test/README *)
real.sml                      
word.sml                      
word8.sml                         (* the C backend does not
	                             implement overflow on
	                             arithmetic operations. *) 

