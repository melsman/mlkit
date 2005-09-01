  val counter = Int.toString
    (case FormVar.wrapOpt FormVar.getIntErr "counter"
       of SOME c => (case Web.Conn.formvar "button"
                       of SOME "Up" => c + 1
                        | SOME "Down" => c - 1
                        | _ => c)
        | NONE => 0)

  val _ = Page.return ("Count: " ^ counter)
    `<form action=counter.sml>
       <input type=hidden name=counter value=^counter>
       <input type=submit name=button value=Up>
       <input type=submit name=button value=Down>
     </form>`
