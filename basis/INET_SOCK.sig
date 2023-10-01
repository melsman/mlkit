(** InetSock interface.

This structure provides operations for creating and manipulating
Internet-domain addresses and sockets.

*)

signature INET_SOCK = sig
  type inet
  type 'st sock = (inet,'st) Socket.sock
  type 'm stream_sock = 'm Socket.stream sock
  type sock_addr = inet Socket.sock_addr

  val inetAF : Socket.AF.addr_family
  val toAddr : NetHostDB.in_addr * int -> sock_addr
  val fromAddr : sock_addr -> NetHostDB.in_addr * int
  val any : int -> sock_addr

  structure TCP : sig
    val socket : unit -> 'm stream_sock
(*
    val socket' : int -> 'm stream_sock
    val getNODELAY : 'm stream_sock -> bool
    val setNODELAY : 'm stream_sock * bool -> unit
*)
  end
end

(**

[type 'st sock] The type of internet sockets.

[type 'm stream_sock] The type of streaming internet sockets.

[type sock_addr] The type of internet socket addresses.

[inetAF] The internet socket address family.

[toAddr(ia,p)] Given an internet address and a port, return a socket
address.

[fromAddr sa] Given a socket address, return the underlying internet
address and port.

[structure TCP] TCP/IP Sockets.

    [socket()] creates a TCP/IP socket.

*)


(** SigDoc *)
structure INetSock : INET_SOCK = INetSock
