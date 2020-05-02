structure Initial2 =
  struct
    exception SysErr of string * int Option.option
    val _ = prim ("sml_setFailNumber", (SysErr ("as",NONE) : exn, 2 : int)) : unit

    local
      fun app f [] = ()
        | app f (x::xs) = (f x ; app f xs)
    in
      fun exitCallback ( i : int) : int =
            if !Initial.exitCalled
            then 1+i
            else
              (Initial.exitCalled := true
              ; app (fn f => (f ()) handle _ => ()) (!Initial.exittasks)
              ; 0+i)
    end

    val _ = _export ("sml_exitCallback", exitCallback)

  end
