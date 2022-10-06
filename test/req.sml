
infix |>
fun x |> f = f x

fun sendVecAll (sock, slc) =
    let val i = Socket.sendVec (sock, slc)
        val len = Word8VectorSlice.length slc
    in if i < len then
         sendVecAll (sock, Word8VectorSlice.subslice(slc, i, NONE))
       else ()
    end

fun req (path:string) (host:string) (port:int): string =
    let val msg =
            String.concat ["GET ", path, " HTTP/1.1\r\n",
                           "Host:", host, "\r\n\r\n"]
                          |> Byte.stringToBytes
        val sock = INetSock.TCP.socket()
        val addr =
            case NetHostDB.getByName host of
                NONE => raise Fail ("unknown host: " ^ host)
              | SOME e => INetSock.toAddr (NetHostDB.addr e, port)
        val bufsz = 2048
        fun loop acc =
            let val v = Socket.recvVec(sock, bufsz)
                val l = Word8Vector.length v
            in if l < bufsz
               then rev (v::acc)
                        |> Word8Vector.concat
                        |> Byte.bytesToString
               else loop (v::acc)
            end
    in Socket.connect (sock, addr)
     ; sendVecAll (sock, Word8VectorSlice.full msg)
     ; loop nil before Socket.close sock
    end

val p = req "/" "www.google.com" 80

val () = print (p ^ "\n")

val () = if String.isSubstring "Google" p then print "OK\n"
         else print "ERR\n"
