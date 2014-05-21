signature EMIT_CODE =
  sig
    type target

    val emit : {target:target, filename:string} -> unit
  end