signature MODCODE_MINI = sig
  type modcode
  type linkinfo
  type target
  val seq : modcode * modcode -> modcode
  val empty : modcode
  val mk_modcode : target * linkinfo * string -> modcode
  val emit : ModuleEnvironments.absprjid * modcode -> modcode
end
