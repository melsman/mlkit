signature DYNLIB =
  sig
    datatype flag = NOW | LAZY
    type ForeignLib
    val dlopen : (string option * flag * bool) -> ForeignLib
    val dlsym  : (string * string * ForeignLib) -> unit
    val isLinked : string -> bool
  end
