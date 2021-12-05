
val () = print "Hello world\n"

val () = print ("Host name: " ^ NetHostDb.getHostName() ^ "\n")

val () =
    case NetHostDb.getByName "localhost" of
        SOME e =>
        let val () = print ("Host: " ^ NetHostDb.name e ^ "\n")
            val () = print "Aliases:\n"
            val () = List.app (fn a => print (a ^ "\n")) (NetHostDb.aliases e)
            val () = print "Addresses:\n"
            val () = List.app (fn a => print (NetHostDb.toString a ^ "\n"))
                              (NetHostDb.addrs e)
        in ()
        end
      | NONE => print "nothing\n"

val ip1 = "127.10.20.13"
val () =
    case NetHostDb.fromString ip1 of
        SOME ia =>
        let val ip2 = NetHostDb.toString ia
        in print ("IP1: " ^ ip1 ^ "\nIP2: " ^ ip2 ^ "\n")
        end
      | NONE => print "Error\n"

val () =
    case NetHostDb.fromString "127.0.0.1" of
        NONE => print "ERR: wrong address\n"
      | SOME a =>
        case NetHostDb.getByAddr a of
            SOME e =>
            let val () = print ("Host: " ^ NetHostDb.name e ^ "\n")
                val () = print "Aliases:\n"
                val () = List.app (fn a => print (a ^ "\n")) (NetHostDb.aliases e)
                val () = print "Addresses:\n"
                val () = List.app (fn a => print (NetHostDb.toString a ^ "\n"))
                                  (NetHostDb.addrs e)
            in ()
            end
          | NONE => print "ERR: no host data\n"
