
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

kitfib35.sml            tx
lexgen.sml              tx 
professor2.sml          tx 
mlyacc.pm               tx 
kitmolgard.sml          tx
kittmergesort.sml       tx 
kitlife35u.sml          tx 
kkb36c.sml              tx 
vliw.sml                tx 
logic.pm
tak.sml
(* tensor.sml        Uses Unsafe.Real64Array and RealArray *)
tailfib.sml
vector-concat.sml
vector-rev.sml
zebra.sml
(* zern.sml          Uses Real64Array, which is unimplemented in ML Kit *)
wc-input1.sml
wc-scanStream.sml
(* tyan.sml          Internal error in ML Kit *)
tsp.sml
(* raytrace.sml      Uses Unsafe.Vector, Posix.Process, Unsafe.CharVector, Real.realMod *)
(* psdes-random.sml  When gc is enabled the executable doesn't terminate *)
peek.sml
(* fxp.sml           Internal error in ML Kit; equality-elimination *)
(* hamlet.sml        Internal error in ML Kit; type name difference *)
mpuz.sml
DLXSimulator.sml
(* md5.sml           Uses Pack32Little *)
matrix-multiply.sml
(*
barnes-hut.pm           tx 
nucleic.pm              tx 
ray.pm                  tx 
ratio-regions.sml       tx 
*)
