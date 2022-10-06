
fun sendHello sock =
    let val bind_addr = Socket.Ctl.getSockName sock
        val bind_pair = INetSock.fromAddr bind_addr
        val peer_addr = Socket.Ctl.getPeerName sock
        val peer_pair = INetSock.fromAddr peer_addr
        fun pr (inaddr,port) = NetHostDB.toString inaddr ^ ":" ^ Int.toString port
        val t = Time.now()
        val date = Date.fromTimeLocal t
        val date_str = Date.toString date
        val msg = "Hello world! \n" ^
                  "The date is " ^ date_str ^ "... \n" ^
                  "Bound address is " ^ pr bind_pair ^ "... \n" ^
                  "Peer address is " ^ pr peer_pair ^ "... "

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
