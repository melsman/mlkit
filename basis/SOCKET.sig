(** Socket interface *)

signature SOCKET = sig
  type ('af,'st) sock   (* af (addr family) : INetSock.inet or UnixSock.unix *)
  type 'af sock_addr    (* st (socket type) : dgram or stream                *)
  (*type dgram*)
  type 'm stream        (* m  (mode)        : active or passive              *)
  type passive
  type active

  structure AF : sig
    eqtype addr_family
    val list       : unit -> (string * addr_family) list
    val toString   : addr_family -> string
    val fromString : string -> addr_family option
  end

  structure SOCK : sig
    eqtype sock_type
    val stream     : sock_type
    val dgram      : sock_type
    val list       : unit -> (string * sock_type) list
    val toString   : sock_type -> string
    val fromString : string -> sock_type option
  end

  structure Ctl : sig
    val getDEBUG     : ('af, 'st) sock -> bool
    val setDEBUG     : ('af, 'st) sock * bool -> unit
    val getREUSEADDR : ('af, 'st) sock -> bool
    val setREUSEADDR : ('af,'st) sock * bool -> unit
    val getKEEPALIVE : ('af, 'st) sock -> bool
    val setKEEPALIVE : ('af, 'st) sock * bool -> unit
    val getDONTROUTE : ('af, 'st) sock -> bool
    val setDONTROUTE : ('af, 'st) sock * bool -> unit
(*
    val getLINGER    : ('af, 'st) sock -> Time.time option
    val setLINGER    : ('af, 'st) sock * Time.time option -> unit
*)
    val getBROADCAST : ('af, 'st) sock -> bool
    val setBROADCAST : ('af, 'st) sock * bool -> unit
    val getOOBINLINE : ('af, 'st) sock -> bool
    val setOOBINLINE : ('af, 'st) sock * bool -> unit
    val getSNDBUF    : ('af, 'st) sock -> int
    val setSNDBUF    : ('af, 'st) sock * int -> unit
    val getRCVBUF    : ('af, 'st) sock -> int
    val setRCVBUF    : ('af, 'st) sock * int -> unit
    val getTYPE      : ('af, 'st) sock -> SOCK.sock_type
    val getERROR     : ('af, 'st) sock -> bool
(*
    val getPeerName  : ('af, 'st) sock -> 'af sock_addr
    val getSockName  : ('af, 'st) sock -> 'af sock_addr
    val getNREAD     : ('af, 'st) sock -> int
    val getATMARK    : ('af, active stream) sock -> bool
*)
  end

  val sameAddr : 'af sock_addr * 'af sock_addr -> bool
  val familyOfAddr : 'af sock_addr -> AF.addr_family

  val bind    : ('af, 'st) sock * 'af sock_addr -> unit
  val listen  : ('af, passive stream) sock * int -> unit
  val accept  : ('af, passive stream) sock
                -> ('af, active stream) sock * 'af sock_addr
(*
  val acceptNB : ('af, passive stream) sock
                   -> (('af, active stream) sock
                       * 'af sock_addr) option
  val connect : ('af, 'st) sock * 'af sock_addr -> unit
  val connectNB : ('af, 'st) sock * 'af sock_addr -> bool
*)

  val close   : ('af, 'st) sock -> unit

  datatype shutdown_mode
      = NO_RECVS
      | NO_SENDS
      | NO_RECVS_OR_SENDS
  val shutdown : ('af, 'mode stream) sock * shutdown_mode -> unit

  type sock_desc
  val sockDesc : ('af, 'st) sock -> sock_desc
  val sameDesc : sock_desc * sock_desc -> bool

  val select : { rds : sock_desc list,
                 wrs : sock_desc list,
                 exs : sock_desc list,
                 timeout : Time.time option
               }
               -> { rds : sock_desc list,
                    wrs : sock_desc list,
                    exs : sock_desc list
                  }

  val ioDesc : ('af, 'st) sock -> OS.IO.iodesc

  type out_flags = {don't_route : bool, oob : bool}
  type in_flags = {peek : bool, oob : bool}

  val sendVec : ('af, active stream) sock * Word8VectorSlice.slice -> int
  val sendArr : ('af, active stream) sock * Word8ArraySlice.slice -> int

(*
  val sendVec' : ('af, active stream) sock * Word8VectorSlice.slice * out_flags -> int
  val sendArr' : ('af, active stream) sock * Word8ArraySlice.slice * out_flags -> int
  val sendVecNB  : ('af, active stream) sock * Word8VectorSlice.slice -> int option
  val sendVecNB' : ('af, active stream) sock * Word8VectorSlice.slice * out_flags -> int option
  val sendArrNB  : ('af, active stream) sock * Word8ArraySlice.slice -> int option
  val sendArrNB' : ('af, active stream) sock * Word8ArraySlice.slice * out_flags -> int option
*)

  val recvVec  : ('af, active stream) sock * int -> Word8Vector.vector

(*
  val recvVec' : ('af, active stream) sock * int * in_flags -> Word8Vector.vector
  val recvArr  : ('af, active stream) sock * Word8ArraySlice.slice -> int
  val recvArr' : ('af, active stream) sock * Word8ArraySlice.slice * in_flags -> int
  val recvVecNB  : ('af, active stream) sock * int -> Word8Vector.vector option
  val recvVecNB' : ('af, active stream) sock * int * in_flags -> Word8Vector.vector option
  val recvArrNB  : ('af, active stream) sock * Word8ArraySlice.slice -> int option
  val recvArrNB' : ('af, active stream) sock * Word8ArraySlice.slice * in_flags -> int option

  val sendVecTo : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice -> unit
  val sendArrTo : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice -> unit
  val sendVecTo' : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice * out_flags -> unit
  val sendArrTo' : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice * out_flags -> unit
  val sendVecToNB  : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice -> bool
  val sendVecToNB' : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice * out_flags -> bool
  val sendArrToNB  : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice -> bool
  val sendArrToNB' : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice * out_flags -> bool

  val recvVecFrom  : ('af, dgram) sock * int -> Word8Vector.vector * 'st sock_addr
  val recvVecFrom' : ('af, dgram) sock * int * in_flags -> Word8Vector.vector * 'st sock_addr
  val recvArrFrom  : ('af, dgram) sock * Word8ArraySlice.slice -> int * 'af sock_addr
  val recvArrFrom' : ('af, dgram) sock * Word8ArraySlice.slice * in_flags -> int * 'af sock_addr
  val recvVecFromNB  : ('af, dgram) sock * int -> (Word8Vector.vector * 'st sock_addr) option
  val recvVecFromNB' : ('af, dgram) sock * int * in_flags ->
                         (Word8Vector.vector * 'st sock_addr) option
  val recvArrFromNB  : ('af, dgram) sock * Word8ArraySlice.slice -> (int * 'af sock_addr) option
  val recvArrFromNB' : ('af, dgram) sock * Word8ArraySlice.slice * in_flags
                         -> (int * 'af sock_addr) option
*)
end

(**

Description:

type ('af,'st) sock
    The type of a socket. Sockets are polymorphic over both the address family
    and the socket type. The type parameter 'af is instantiated with the
    appropriate address family type (INetSock.inet or UnixSock.unix). The type
    parameter 'st is instantiated with the appropriate socket type
    (dgram or stream).

type 'af sock_addr
    The type of a socket address. The type parameter 'af describes the address
    family of the address (INetSock.inet or UnixSock.unix).

type dgram
    The witness type for datagram sockets.

type 'mode stream
    The witness type for stream sockets. The type parameter 'mode describes the
    mode of the stream socket: active or passive.

structure AF
    The AF substructure defines an abstract type that represents the different
    network-address families.

    val list : unit -> (string * addr_family) list
        This returns a list of all the available address families. Every
        element of the list is a pair (name,af) where name is the name of the
        address family, and af is the actual address family value.

        The names of the address families are taken from the symbolic constants
        used in the C Socket API and stripping the leading ``AF_.'' For
        example, the Unix-domain address family is named "UNIX", the
        Internet-domain address family is named "INET", and the Apple Talk
        address family is named "APPLETALK".

    val toString : addr_family -> string
    val fromString : string -> addr_family option
        These convert between address family values and their names. For
        example, the expression toString (INetSock.inetAF) returns the string
        "INET". fromString returns NONE if no family value corresponds to the
        given name.

        If a pair (name,af) is in the list returned by list, then it is the
        case that name is equal to toString(af).

structure SOCK
    The SOCK substructure provides an abstract type and operations for the
    different types of sockets. This type is used by the getTYPE function.

    eqtype sock_type
        The type of socket types.

    val stream : sock_type
        The stream socket type value.

    val dgram : sock_type
        The datagram socket type value.

    val list : unit -> (string * sock_type) list
        A list of the available socket types. Every element of the list is of
        the form (name,sty) where name is the name of the socket type, and sty
        is the actual socket type value.

        The list of possible socket type names includes "STREAM" for stream
        sockets, "DGRAM" for datagram sockets, and "RAW" for raw sockets. These
        names are formed by taking the symbolic constants from the C API and
        removing the leading ``SOCK_.''

    val toString : sock_type -> string
    val fromString : string -> sock_type option
        These convert between a socket type value and its name (e.g.,
        "STREAM"). fromString returns NONE if no socket type value corresponds
        to the name.

        If a pair (name,sty) is in the list returned by list, then it is the
        case that name is equal to toString(sty).

structure Ctl
    The Ctl substructure provides support for manipulating the options
    associated with a socket. These functions raise the SysErr exception when
    the argument socket has been closed.

    val getDEBUG : ('af, 'st) sock -> bool
    val setDEBUG : ('af, 'st) sock * bool -> unit
        These functions query and set the SO_DEBUG flag for the socket. This
        flag enables or disables low-level debugging within the kernel.
        Enabled, it allows the kernel to maintain a history of the recent
        packets that have been received or sent.

    val getREUSEADDR : ('af, 'st) sock -> bool
    val setREUSEADDR : ('af, 'st) sock * bool -> unit
        These query and set the SO_REUSEADDR flag for the socket. When true,
        this flag instructs the system to allow reuse of local socket addresses
        in bind calls.

    val getKEEPALIVE : ('af, 'st) sock -> bool
    val setKEEPALIVE : ('af, 'st) sock * bool -> unit
        These query and set the SO_KEEPALIVE flag for the socket. When true,
        the system will generate periodic transmissions on a connected socket,
        when no other data is being exchanged.

    val getDONTROUTE : ('af, 'st) sock -> bool
    val setDONTROUTE : ('af, 'st) sock * bool -> unit
        These query and set the SO_DONTROUTE flag for the socket. When this
        flag is true, outgoing messages bypass the normal routing mechanisms of
        the underlying protocol, and are instead directed to the appropriate
        network interface as specified by the network portion of the
        destination address. Note that this option can be specified on a per
        message basis by using one of the sendVec', sendArr', sendVecTo', or
        sendArrTo' functions.

    val getLINGER : ('af, 'st) sock -> Time.time option
    val setLINGER : ('af, 'st) sock * Time.time option
                      -> unit
        These functions query and set the SO_LINGER flag for the socket sock.
        This flag controls the action taken when unsent messages are queued on
        socket and a close is performed.  If the flag is set to NONE, then the
        system will close the socket as quickly as possible, discarding data if
        necessary. If the flag is set to SOME(t) and the socket promises
        reliable delivery, then the system will block the close operation until
        the data is delivered or the timeout t expires. If t is negative or too
        large, then the Time is raised.

    val getBROADCAST : ('af, 'st) sock -> bool
    val setBROADCAST : ('af, 'st) sock * bool -> unit
        These query and set the SO_BROADCAST flag for the socket sock, which
        enables or disables the ability of the process to send broadcast
        messages over the socket.

    val getOOBINLINE : ('af, 'st) sock -> bool
    val setOOBINLINE : ('af, 'st) sock * bool -> unit
        These query and set the SO_OOBINLINE flag for the socket. When set,
        this indicates that out-of-band data should be placed in the normal
        input queue of the socket. Note that this option can be specified on a
        per message basis by using one of the sendVec', sendArr', sendVecTo',
        or sendArrTo' functions.

    val getSNDBUF : ('af, 'st) sock -> int
    val setSNDBUF : ('af, 'st) sock * int -> unit
        These query and set the size of the send queue buffer for the socket.

    val getRCVBUF : ('af, 'st) sock -> int
    val setRCVBUF : ('af, 'st) sock * int -> unit
        These query and set the size of receive queue buffer for the socket.

    val getTYPE : ('af, 'st) sock -> SOCK.sock_type
        This returns the socket type of the socket.

    val getERROR : ('af, 'st) sock -> bool
        This indicates whether or not an error has occurred.

    val getPeerName : ('af, 'st) sock -> 'af sock_addr
        This returns the socket address to which the socket is connected.

    val getSockName : ('af, 'st) sock -> 'af sock_addr
        This returns the socket address to which the socket is bound.

    val getNREAD : ('af, 'st) sock -> int
        This returns the number of bytes available for reading on the socket.

    val getATMARK : ('af, active stream) sock -> bool
        This indicates whether or not the read pointer on the socket is
        currently at the out-of-band mark.

val sameAddr : 'af sock_addr * 'af sock_addr -> bool
    This tests whether two socket addresses are the same address.

familyOfAddr addr
    returns the address family of the socket address addr.

bind (sock, sa)
    binds the address sa to the passive socket sock. This function raises
    SysErr when the address sa is already in use, when sock is already bound to
    an address, or when sock has been closed.

listen (sock, n)
    creates a queue (of size n) for pending questions associated to the socket
    sock. The size of queue is limited by the underlying system, but requesting
    a queue size larger than the limit does not cause an error (a typical limit
    is 128, but older systems use a limit of 5).

    This function raises the SysErr exception if sock has been closed.

accept sock
    extracts the first connection request from the queue of pending connections
    for the socket sock. The socket must have been bound to an address via bind
    and enabled for listening via listen. If a connection is present, accept
    returns a pair (s,sa) consisting of a new active socket s with the same
    properties as sock and the address sa of the connecting entity. If no
    pending connections are present on the queue then accept blocks until a
    connection is requested. One can test for pending connection requests by
    using the select function to test the socket for reading.

    This function raises the SysErr exception if sock has not been properly
    bound and enabled, or it sock has been closed.

val acceptNB : ('af, passive stream) sock
                 -> (('af, active stream) sock
                 * 'af sock_addr) option
    This function is the nonblocking form of the accept operation. If the
    operation can complete without blocking (i.e., there is a pending
    connection), then this function returns SOME(s,sa), where s is a new active
    socket with the same properties as sock and sa is the the address of the
    connecting entity. If there are no pending connections, then this function
    returns NONE.

    This function raises the SysErr exception if sock has not been properly
    bound and enabled, or it sock has been closed.

connect (sock, sa)
    attempts to connect the socket sock to the address sa. If sock is a
    datagram socket, the address specifies the peer with which the socket is to
    be associated; sa is the address to which datagrams are to be sent, and the
    only address from which datagrams are to be received.  If sock is a stream
    socket, the address specifies another socket to which to connect.

    This function raises the SysErr exception when the address specified by sa
    is unreachable, when the connection is refused or times out, when sock is
    already connected, or when sock has been closed.

val connectNB : ('af, 'st) sock * 'af sock_addr
                  -> bool
    This function is the nonblocking form of connect. If the connection can be
    established without blocking the caller (which is typically true for
    datagram sockets, but not stream sockets), then true is returned.
    Otherwise, false is returned and the connection attempt is started; one can
    test for the completion of the connection by testing the socket for writing
    using the select function. This function will raise SysErr if it is called
    on a socket for which a previous connection attempt has not yet been
    completed.

close sock
    closes the connection to the socket sock. This function raises the SysErr
    exception if the socket has already been closed.

shutdown (sock, mode)
    shuts down all or part of a full-duplex connection on socket sock. If mode
    is NO_RECVS, further receives will be disallowed. If mode is NO_SENDS,
    further sends will be disallowed. If mode is NO_RECVS_OR_SENDS, further
    sends and receives will be disallowed. This function raises the SysErr
    exception if the socket is not connected or has been closed.

type sock_desc
    This type is an abstract name for a socket, which is used to support
    polling on collections of sockets.

sockDesc sock
    returns a socket descriptor that names the socket sock.

sameDesc (sd1, sd2)
    returns true if the two socket descriptors sd1 and sd2 describe the same
    underlying socket. Thus, the expression sameDesc(sockDesc sock, sockDesc
    sock) will always return true for any socket sock.

select {rds, wrs, exs, timeout}
    examines the sockets in rds, wrs, and exs to see if they are ready for
    reading, writing, or have an exceptional condition pending, respectively.
    The calling program is blocked until either one or more of the named
    sockets is ``ready '' or the specified timeout expires (where a timeout of
    NONE never expires). The result of select is a record of three lists of
    socket descriptors containing the ready sockets from the corresponding
    argument lists. The order in which socket descriptors appear in the
    argument lists is preserved in the result lists. A timeout is signified by
    a result of three empty lists.

    This function raises SysErr if any of the argument sockets have been closed
    or if the timeout value is negative.

    Note that one can test if a call to accept will block by using select to
    see if the socket is ready to read. Similarly, one can use select to test
    if a call to connect will block by seeing if the socket is ready to write.

ioDesc sock
    returns the I/O descriptor corresponding to socket sock. This descriptor
    can be used to poll the socket via pollDesc and poll in the OS.IO
    structure. Using the polling mechanism from OS.IO has the advantage that
    different kinds of I/O objects can be mixed, but not all systems support
    polling on sockets this way. If an application is only polling sockets,
    then it is more portable to use the select function defined above.

type out_flags = {don't_route : bool, oob : bool}
    Flags used in the general form of socket output operations.

type in_flags = {peek : bool, oob : bool}
    Flags used in the general form of socket input operations.

sendVec (sock, slice)
sendArr (sock, slice)
    These functions send the bytes in the slice slice on the active stream
    socket sock. They return the number of bytes actually sent.

    These functions raise SysErr if sock has been closed.

sendVec' (sock, slice, {don't_route, oob})
sendArr' (sock, slice, {don't_route, oob})
    These functions send the bytes in the slice slice on the active stream
    socket sock. They return the number of bytes actually sent. If the
    don't_route flag is true, the data is sent bypassing the normal routing
    mechanism of the protocol. If oob is true, the data is sent out-of-band,
    that is, before any other data which may have been buffered.

    These functions raise SysErr if sock has been closed.

val sendVecNB : ('af, active stream) sock
                  * Word8VectorSlice.slice -> int option
val sendVecNB' : ('af, active stream) sock
                   * Word8VectorSlice.slice
                   * out_flags -> int option
val sendArrNB : ('af, active stream) sock
                  * Word8ArraySlice.slice -> int option
val sendArrNB' : ('af, active stream) sock
                   * Word8ArraySlice.slice
                   * out_flags -> int option
    These functions are the nonblocking versions of sendVec, sendVec', sendArr,
    and sendArr' (resp.). They have the same semantics as their blocking forms,
    with the exception that when the operation can complete without blocking,
    then the result is wrapped in SOME and if the operation would have to wait
    to send the data, then NONE is returned instead.

recvVec (sock, n)
recvVec'(sock, n, {peek,oob})
    These functions receive up to n bytes from the active stream socket sock.
    The size of the resulting vector is the number of bytes that were
    successfully received, which may be less than n. If the connection has been
    closed at the other end (or if n is 0), then the empty vector will be
    returned.

    In the second version, if peek is true, the data is received but not
    discarded from the connection. If oob is true, the data is received
    out-of-band, that is, before any other incoming data that may have been
    buffered.

    These functions raise SysErr if the socket sock has been closed and they
    raise Size if n < 0 or n > Word8Vector.maxLen.

recvArr (sock, slice)
recvArr' (sock, slice, {peek, oob})
    These functions read data from the socket sock into the array slice slice.
    They return the number of bytes actually received. If the connection has
    been closed at the other end or the slice is empty, then 0 is returned.

    For recvArr', if peek is true, the data is received but not discarded from
    the connection.  If oob is true, the data is received out-of-band, that is,
    before any other incoming data that may have been buffered.

    These functions raise SysErr if sock has been closed.

val recvVecNB : ('af, active stream) sock * int
                  -> Word8Vector.vector option
val recvVecNB' : ('af, active stream) sock * int * in_flags
                   -> Word8Vector.vector option
val recvArrNB : ('af, active stream) sock
                  * Word8ArraySlice.slice -> int option
val recvArrNB' : ('af, active stream) sock
                   * Word8ArraySlice.slice
                   * in_flags -> int option
    These functions are the nonblocking versions of recvVec, recvVec', recvArr,
    and recvArr' (resp.). They have the same semantics as their blocking forms,
    with the exception that when the operation can complete without blocking,
    then the result is wrapped in SOME and if the operation would have to wait
    for input, then NONE is returned instead.

sendVecTo (sock, sa, slice)
sendArrTo (sock, sa, slice)
    These functions send the message specified by the slice slice on the
    datagram socket sock to the address sa.

    These functions raise SysErr if sock has been closed or if the socket has
    been connected to a different address than sa.

sendVecTo' (sock, sa, slice, {don't_route, oob})
sendArrTo' (sock, sa, slice, {don't_route, oob})
    These functions send the message specified by the slice slice on the
    datagram socket sock to the address

    If the don't_route flag is true, the data is sent bypassing the normal
    routing mechanism of the protocol. If oob is true, the data is sent
    out-of-band, that is, before any other data which may have been buffered.

    These functions raise SysErr if sock has been closed or if the socket has
    been connected to a different address than sa.

val sendVecToNB : ('af, dgram) sock
                    * 'af sock_addr
                    * Word8VectorSlice.slice -> bool
val sendVecToNB' : ('af, dgram) sock
                     * 'af sock_addr
                     * Word8VectorSlice.slice
                     * out_flags -> bool
val sendArrToNB : ('af, dgram) sock
                    * 'af sock_addr
                    * Word8ArraySlice.slice -> bool
val sendArrToNB' : ('af, dgram) sock
                     * 'af sock_addr
                     * Word8ArraySlice.slice
                     * out_flags -> bool
    These functions are the nonblocking versions of sendVecTo, sendVecTo',
    sendArrTo, and sendArrTo' (resp.). They have the same semantics as their
    blocking forms, with the exception that if the operation can complete
    without blocking, then the operation is performed and true is returned.
    Otherwise, false is returned and the message is not sent.

recvVecFrom (sock, n)
recvVecFrom' (sock, n, {peek, oob})
    These functions receive up to n bytes on the datagram socket sock, and
    return a pair (vec,sa), where the vector vec is the received message, and
    sa is the socket address from the which the data originated. If the message
    is larger than n, then data may be lost.

    In the second form, if peek is true, the data is received but not discarded
    from the connection. If oob is true, the data is received out-of-band, that
    is, before any other incoming data that may have been buffered.

    These functions raise SysErr if sock has been closed; they raise Size if n
    < 0 or n > Word8Vector.maxLen.

recvArrFrom (sock, slice)
recvArrFrom' (sock, slice)
    These functions read a message from the datagram socket sock into the array
    slice slice.  If the message is larger than the size of the slice, then
    data may be lost. They return the number of bytes actually received. If the
    connection has been closed at the other end or the slice is empty, then 0
    is returned.

    For recvArrFrom', if peek is true, the data is received but not discarded
    from the connection. If oob is true, the data is received out-of-band, that
    is, before any other incoming data that may have been buffered.

    These functions raise SysErr if sock has been closed.

val recvVecFromNB : ('af, dgram) sock * int
                      -> (Word8Vector.vector
                      * 'st sock_addr) option
val recvVecFromNB' : ('af, dgram) sock * int * in_flags
                       -> (Word8Vector.vector
                       * 'st sock_addr) option
val recvArrFromNB : ('af, dgram) sock
                      * Word8ArraySlice.slice
                      -> (int * 'af sock_addr) option
val recvArrFromNB' : ('af, dgram) sock
                       * Word8ArraySlice.slice
                       * in_flags
                       -> (int * 'af sock_addr) option
    These functions are the nonblocking versions of recvVecFrom, recvVecFrom',
    recvArrFrom, and recvArrFrom' (resp.). They have the same semantics as
    their blocking forms, with the exception that when the operation can
    complete without blocking, then the result is wrapped in SOME and if the
    operation would have to wait for input, then NONE is returned instead.

*)
