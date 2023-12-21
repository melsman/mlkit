
structure StringSet : KIT_MONO_SET =
  OrderSet (struct type t = string val lt : t*t -> bool = op < end)
