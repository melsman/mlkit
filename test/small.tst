
(* Benchmark file *)

kitfib35.sml           mlton
lexgen.sml
professor2.sml
mlyacc.pm
kitmolgard.sml
kittmergesort.sml
kitlife35u.sml         mlton
kkb36c.sml             mlton
vliw.sml
logic.pm
tak.sml
tailfib.sml
vector-concat.sml
vector-rev.sml
zebra.sml
wc-input1.sml
wc-scanStream.sml
tyan.sml
tsp.sml
peek.sml
mpuz.sml
DLXSimulator.sml
(* md5.sml                  out-of-memory without GC *)
(* checksum.sml *)
(* smith-normal-form.sml    out-of-memory without GC *)
(* matrix-multiply.sml      out-of-memory without GC *)
ratio-regions.sml
(*
barnes-hut.pm
nucleic.pm
ray.pm
*)

(* zern.sml          Uses Real64Array, which is unimplemented in ML Kit *)
(* tensor.sml        Uses Unsafe.Real64Array and RealArray *)
(* raytrace.sml      Uses Unsafe.Vector, Posix.Process, Unsafe.CharVector, Real.realMod *)

(* psdes-random.sml  When gc is enabled the executable doesn't terminate -- problem with tail-calls *)
(* hamlet.sml        Internal error in ML Kit; problem with Opacity Elimination -- type name difference *)

(* fxp.sml           Main structure missing *)
