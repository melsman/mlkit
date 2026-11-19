signature RECORD_FIELD =
sig
  type mode = {mutable: bool, traversable: bool}
  val ordinary : mode
  val mutable : mode
  val raw : mode
  val validPrefix : mode list -> bool
  val rawPrefix : mode list -> int
  val layout : mode -> string
  val pu : mode Pickle.pu
end
