signature GENERIC_SOCK =
  sig
    val socket : Socket.AF.addr_family * Socket.SOCK.sock_type
                   -> ('af, 'sock_type) Socket.sock
    val socketPair : Socket.AF.addr_family
                       * Socket.SOCK.sock_type
                       -> ('af, 'sock_type) Socket.sock
                       * ('af, 'sock_type) Socket.sock
    val socket' : Socket.AF.addr_family
                    * Socket.SOCK.sock_type
                    * int -> ('af, 'sock_type) Socket.sock
    val socketPair' : Socket.AF.addr_family
                        * Socket.SOCK.sock_type
                        * int
                        -> ('af, 'sock_type) Socket.sock
                        * ('af, 'sock_type) Socket.sock
  end

(*
socket (af, st)
    creates a socket in the address family specified by af and the socket type
    specified by st, with the default protocol.

socketPair (af, st)
    creates an unnamed pair of connected sockets in the address family
    specified by af and the socket type specified by st, with the default
    protocol.

socket' (af, st, i)
    creates a socket in the address family specified by af and the socket type
    specified by st, with protocol number i.

socketPair' (af, st, i)
    creates an unnamed pair of connected sockets in the address family
    specified by af and the socket type specified by st, with protocol number
    i.
*)
