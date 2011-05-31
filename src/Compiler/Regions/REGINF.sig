(*$REGINF:*)
signature REGINF =            (***************)
  sig                         (* Algorithm R *)
                              (* Mads Tofte  *)
    type cone                 (***************)
    type rse
    type place
    type ('a,'b)trip

    val inferEffects : (string -> unit) -> cone * rse * (place,unit)trip -> cone
  end
