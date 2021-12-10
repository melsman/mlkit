
val () = print "Hello world\n"

val () = print ("Host name: " ^ NetHostDB.getHostName() ^ "\n")

val () =
    case NetHostDB.getByName "localhost" of
        SOME e =>
        let val () = print ("Host: " ^ NetHostDB.name e ^ "\n")
            val () = print "Aliases:\n"
            val () = List.app (fn a => print (a ^ "\n")) (NetHostDB.aliases e)
            val () = print "Addresses:\n"
            val () = List.app (fn a => print (NetHostDB.toString a ^ "\n"))
                              (NetHostDB.addrs e)
        in ()
        end
      | NONE => print "nothing\n"

val ip1 = "127.10.20.13"
val () =
    case NetHostDB.fromString ip1 of
        SOME ia =>
        let val ip2 = NetHostDB.toString ia
        in print ("IP1: " ^ ip1 ^ "\nIP2: " ^ ip2 ^ "\n")
        end
      | NONE => print "Error\n"

val () =
    case NetHostDB.fromString "127.0.0.1" of
        NONE => print "ERR: wrong address\n"
      | SOME a =>
        case NetHostDB.getByAddr a of
            SOME e =>
            let val () = print ("Host: " ^ NetHostDB.name e ^ "\n")
                val () = print "Aliases:\n"
                val () = List.app (fn a => print (a ^ "\n")) (NetHostDB.aliases e)
                val () = print "Addresses:\n"
                val () = List.app (fn a => print (NetHostDB.toString a ^ "\n"))
                                  (NetHostDB.addrs e)
            in ()
            end
          | NONE => print "ERR: no host data\n"
