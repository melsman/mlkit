(* Retain the legacy JavaScript REAL conversion aliases for narrow families. *)
structure Real16 =
struct
  open Real16
  val toDefault = toLarge
  val fromDefault = fromLarge IEEEReal.TO_NEAREST
end
structure Real32 =
struct
  open Real32
  val toDefault = toLarge
  val fromDefault = fromLarge IEEEReal.TO_NEAREST
end
