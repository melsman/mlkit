signature UNIX_SOCK = 
  sig
    type unix
    type 'sock_type sock = (unix, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock
    type dgram_sock = Socket.dgram sock
    type sock_addr = unix Socket.sock_addr
    val unixAF : Socket.AF.addr_family
    val toAddr : string -> sock_addr
    val fromAddr : sock_addr -> string
    structure Strm : sig
        val socket : unit -> 'mode stream_sock
        val socketPair : unit
                           -> 'mode stream_sock * 'mode stream_sock
      end
    structure DGrm : sig
        val socket : unit -> dgram_sock
        val socketPair : unit -> dgram_sock * dgram_sock
      end 
  end

(*
type unix
    The witness type of the Unix address family.

type 'sock_type sock = (unix, 'sock_type) Socket.sock
    The type-scheme for all Unix-domain sockets.

type 'mode stream_sock = 'mode Socket.stream sock
    The type-scheme of Unix-domain (passive or active) stream sockets.

type dgram_sock = Socket.dgram sock
    The type of Unix-domain datagram sockets.

type sock_addr = unix Socket.sock_addr
    The type of a Unix-domain socket address.

val unixAF : Socket.AF.addr_family
    The Unix address family value.

toAddr s
    converts a pathname s into a socket address (in the Unix address family);
    it does not check the validity of the path s.

fromAddr addr
    returns the Unix file system path corresponding to the Unix-domain socket
    address addr.

structure Strm

    val socket : unit -> 'mode stream_sock
        This function creates a stream socket in the Unix address family. It
        raises SysErr if there are too many sockets in use.

    val socketPair : unit
                       -> 'mode stream_sock * 'mode stream_sock
        This function creates an unnamed pair of connected stream sockets in
        the Unix address family. It is similar to the Posix.IO.pipe function in
        that the returned sockets are connected, but unlike pipe, the sockets
        are bidirectional. It raises SysErr if there are too many sockets in
        use.

structure DGrm

    val socket : unit -> dgram_sock
        This function creates a datagram socket in the Unix address family. It
        raises SysErr if there are too many sockets in use.

    val socketPair : unit -> dgram_sock * dgram_sock
        This function creates an unnamed pair of connected datagram sockets in
        the Unix address family. It raises SysErr if there are too many sockets
        in use.
*)
