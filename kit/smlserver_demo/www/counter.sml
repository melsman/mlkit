  val counter = Int.toString
    (case FormVar.getInt "counter"
       of SOME c => (case Ns.Conn.formvar "button"
                       of SOME "Up" => c + 1
                        | SOME "Down" => c - 1
                        | _ => c)
        | NONE => 0)

  val _ = Ns.Quot.return `
    <html>
      <body bgcolor=white>
        <h2>Count: ^counter</h2>
        <form action=counter.sml>
          <input type=hidden name=counter value=^counter>
          <input type=submit name=button value=Up>
          <input type=submit name=button value=Down>
        </form>
        <hr> <i>Served by SMLserver</i>
      </body>
    </html>`
