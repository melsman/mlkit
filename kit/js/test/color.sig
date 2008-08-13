signature COLOR = sig
  eqtype t
  val rgb : int * int * int -> t

  val fromHex : word -> t
  val fromString : string -> t option
  val toString   : t -> string

  val aqua    : t
  val black   : t
  val blue    : t
  val fuchsia : t
  val gray    : t
  val green   : t
  val lime    : t
  val maroon  : t
  val navy    : t
  val olive   : t
  val purple  : t
  val red     : t
  val silver  : t
  val teal    : t
  val white   : t
  val yellow  : t
end
