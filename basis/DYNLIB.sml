signature DYNLIB =
  sig
    datatype flag = NOW | LAZY
    type ForeignLib
    val dlopen : (string option * flag * bool) -> ForeignLib
    val dlsym  : (string * string * ForeignLib) -> unit
    val isLinked : string -> bool
  end

(* Dynamic Linking interface

   [dlopen(SOME lib, f, g)] opens the shared library lib using the dlopen
   supplied by the linker of the operating system with the flags given in f and
   g. If g then the global flag is set.

   [dlopen(NONE,v,g)] Like above, where the library is the main program.

   [dlsym(ml,c,h)] Associate the c-function c in the library represented by h
   with the primitive name ml. After this call you can make primitive calls to
   the c-fuction by prim(":",..) where the first element in the tuple .. must
   be ml.

   [isLinked ml] return true iff ml is associated to a c-function in a library.

*)
