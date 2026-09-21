signature INSTS_ARM64 = sig
  include INSTS_BASE
  type lvar
  datatype reg = X of int | D of int | SP
  datatype inst = Label of lab | Directive of string | Op of string * string list
  type AsmPrg = inst list
  val pr_reg : reg -> string
  val optimise : AsmPrg -> AsmPrg
  val emit : AsmPrg * string -> unit
  structure RI : REGISTER_INFO where type reg = reg where type lvar = lvar
end
