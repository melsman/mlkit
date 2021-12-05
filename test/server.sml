
fun sendHello sock =
    let
        val t = Time.now()
        val date = Date.fromTimeLocal t
        val date_str = Date.toString date
        val msg = "Hello world! " ^
                  "The date is " ^ date_str ^ "..."
        val res = "HTTP/1.1 200 OK\r\nContent-Length: " ^ Int.toString (size msg) ^
                  "\r\n\r\n" ^ msg ^ "\r\n\r\n"
        val slc = Word8VectorSlice.full (Byte.stringToBytes res)
    in
      Socket.sendVec (sock, slc);
      Socket.close sock
    end

fun acceptLoop serv : unit =
    let val (s, _) = Socket.accept serv
    in print "Accepted a connection...\n";
       sendHello s;
       acceptLoop serv
    end

fun serve p : unit =
    let val s = INetSock.TCP.socket()
    in Socket.Ctl.setREUSEADDR (s, true);
       Socket.bind(s, INetSock.any p);
       Socket.listen(s, 5);
       print ("HTTP server started on port " ^ Int.toString p ^ "\n");
       print ("Use C-c to exit the request loop...\n");
       acceptLoop s
    end

val () = serve 8989
