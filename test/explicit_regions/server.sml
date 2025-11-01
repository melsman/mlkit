
fun copyString `[r1 r2] (s:string`r1) : string`r2 = s ^ ""

fun resp `r (data: string`r) : unit =
    print ("Response sent to client: " ^ data)

fun service `[r1 r2] () : unit =
    let
      val data = "Hello, this is a chat message. \n" : string`r1
      val response : string`r2 = copyString `[r1 r2] data
    in
      resp response
    end

fun run () =
  ( print "Starting mock server\n"
  ; service ()
  ; print "Server stopped\n"
  )

val () = run()
