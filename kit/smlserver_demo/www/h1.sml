open Msp

infix &&

val body = h1($"Your Main Page") &&
           $"Hi " && $(%"name") && $" - how are you?"

val page = html (bodya "bgcolor=white" body)

val _ = Ns.Conn.return (flatten page)