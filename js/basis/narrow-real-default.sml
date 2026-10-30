(* Retain legacy JavaScript conversion aliases alongside the shared REAL interface. *)
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

structure Real =
struct
  open Real
  fun toDefault x = x
  fun fromDefault x = x
end
structure Real64 = Real
structure LargeReal = Real
val floor = Real.floor
val ceil = Real.ceil
val trunc = Real.trunc
val round = Real.round
