local

  fun not_impl s = raise Fail ("not implemented: " ^ s)

  fun getCtx () : foreignptr = prim("__get_ctx",())

  (* error utilities *)

  fun failure s =
      let fun errno () : int = prim("sml_errno",())
          fun errmsg (i : int) : string = prim("sml_errormsg", i)
      in raise Fail (s ^ ": " ^ errmsg(errno()))
      end

  fun maybe_failure s i =
      if i < 0 then failure s
      else ()

  structure Socket : sig
    datatype af = Inet_af | Unix_af
    datatype sock_addr0 =
           Inet_sa of {addr:int,port:int}
         | Unix_sa of {name:string}
    type sock0 = {fd:int,af:af}
    include SOCKET
      where type ('af,'sd) sock = sock0
      where type 'af sock_addr = sock_addr0
    val INADDR_ANY : int
    val AF_INET : int
    val SO_REUSEADDR : int
    val SOCK_STREAM : int
  end = struct

    (* see socket.c *)

    val { AF_INET      : int
        , AF_UNIX      : int
        , INADDR_ANY   : int
        , SHUT_RD      : int
        , SHUT_RDWR    : int
        , SHUT_WR      : int
        , SOCK_DGRAM   : int
        , SOCK_RAW     : int
        , SOCK_STREAM  : int
        , SO_BROADCAST : int
        , SO_DEBUG     : int
        , SO_DONTROUTE : int
        , SO_ERROR     : int
        , SO_KEEPALIVE : int
        , SO_LINGER    : int
        , SO_OOBINLINE : int
        , SO_RCVBUF    : int
        , SO_REUSEADDR : int
        , SO_SNDBUF    : int
        , SO_TYPE      : int
        } = prim("sml_sock_getDefines",())

    datatype af = Inet_af | Unix_af

    type sock0 = {fd:int, af:af}

    datatype sock_addr0 =
             Inet_sa of {addr:int,port:int}
           | Unix_sa of {name:string}

    type ('af,'st) sock = sock0
    type 'af sock_addr = sock_addr0
    datatype 'm stream = STREAM
    datatype passive = PASSIVE
    datatype active = ACTIVE

    structure AF : sig
      type addr_family
      val list       : unit -> (string * addr_family) list
      val toString   : addr_family -> string
      val fromString : string -> addr_family option
    end = struct
      type addr_family = int
      fun list () =
          [("INET", AF_INET),
           ("UNIX", AF_UNIX)]
      fun toString i =
          if i = AF_INET then "INET"
          else if i = AF_UNIX then "UNIX"
          else raise Fail "Socket.AF.toString: impossible"
      fun fromString "INET" = SOME AF_INET
        | fromString "UNIX" = SOME AF_UNIX
        | fromString _ = NONE
    end

    structure SOCK : sig
      eqtype sock_type
      val stream     : sock_type
      val dgram      : sock_type
      val list       : unit -> (string * sock_type) list
      val toString   : sock_type -> string
      val fromString : string -> sock_type option
    end = struct
      type sock_type = int
      val stream = SOCK_STREAM
      val dgram = SOCK_DGRAM
      fun list () = [("DGRAM",SOCK_DGRAM),
                     ("RAW",SOCK_RAW),
                     ("STREAM",SOCK_STREAM)]
      fun toString i =
          if i = SOCK_DGRAM then "DGRAM"
          else if i = SOCK_RAW then "RAM"
          else if i = SOCK_STREAM then "STREAM"
          else raise Fail "Socket.SOCK.toString: impossible"
      fun fromString "DGRAM" = SOME SOCK_DGRAM
        | fromString "RAW" = SOME SOCK_RAW
        | fromString "STREAM" = SOME SOCK_STREAM
        | fromString _ = NONE
    end

    structure Ctl = struct
      fun getSockOptInt0 (s:('af, 'st) sock, opt:int) : int =
          prim("sml_sock_getsockopt", (#fd s,opt))

      fun getSockOptInt (opt:int) str (s:('af, 'st) sock)  : int =
          let val ret = getSockOptInt0(s,opt)
          in maybe_failure str ret
           ; ret
          end

      fun setSockOptInt (opt:int) (str:string) (s:('af, 'st) sock, v:int) : unit =
          let val ret = prim("sml_sock_setsockopt", (#fd s,opt,v))
          in maybe_failure str ret
          end

      fun getSockOptBool (opt:int) (str:string) (s:('af, 'st) sock) : bool =
          let val ret = getSockOptInt0(s,opt)
          in maybe_failure str ret
           ; ret > 0
          end

      fun setSockOptBool (opt:int) (str:string) (s:('af, 'st) sock, b:bool) : unit =
          let val ret = prim("sml_sock_setsockopt", (#fd s,opt,b))
          in maybe_failure str ret
          end

      fun getDEBUG s = getSockOptBool SO_DEBUG "Socket.Ctl.getDEBUG" s
      fun setDEBUG (s,b) = setSockOptBool SO_DEBUG "Socket.Ctl.setDEBUG" (s,b)

      fun getREUSEADDR s = getSockOptBool SO_REUSEADDR "Socket.Ctl.getREUSEADDR" s
      fun setREUSEADDR (s,b) = setSockOptBool SO_REUSEADDR "Socket.Ctl.setREUSEADDR" (s,b)

      fun getKEEPALIVE s = getSockOptBool SO_KEEPALIVE "Socket.Ctl.getKEEPALIVE" s
      fun setKEEPALIVE (s,b) = setSockOptBool SO_KEEPALIVE "Socket.Ctl.setKEEPALIVE" (s,b)

      fun getDONTROUTE s = getSockOptBool SO_DONTROUTE "Socket.Ctl.getDONTROUTE" s
      fun setDONTROUTE (s,b) = setSockOptBool SO_DONTROUTE "Socket.Ctl.setDONTROUTE" (s,b)

      fun getBROADCAST s = getSockOptBool SO_BROADCAST "Socket.Ctl.getBROADCAST" s
      fun setBROADCAST (s,b) = setSockOptBool SO_BROADCAST "Socket.Ctl.setBROADCAST" (s,b)

      fun getOOBINLINE s = getSockOptBool SO_OOBINLINE "Socket.Ctl.getOOBINLINE" s
      fun setOOBINLINE (s,b) = setSockOptBool SO_OOBINLINE "Socket.Ctl.setOOBINLINE" (s,b)

      fun getERROR s = getSockOptBool SO_ERROR "Socket.Ctl.getERROR" s

(*
    val getLINGER    : ('af, 'st) sock -> Time.time option
    val setLINGER    : ('af, 'st) sock * Time.time option -> unit
*)

      fun getSNDBUF s = getSockOptInt SO_SNDBUF "Socket.Ctl.getSNDBUF" s
      fun setSNDBUF (s,b) = setSockOptInt SO_SNDBUF "Socket.Ctl.setSNDBUF" (s,b)

      fun getRCVBUF s = getSockOptInt SO_RCVBUF "Socket.Ctl.getRCVBUF" s
      fun setRCVBUF (s,b) = setSockOptInt SO_RCVBUF "Socket.Ctl.setRCVBUF" (s,b)

      fun getTYPE s : SOCK.sock_type = getSockOptInt SO_TYPE "Socket.Ctl.getTYPE" s

(*
    val getPeerName  : ('af, 'st) sock -> 'af sock_addr
    val getSockName  : ('af, 'st) sock -> 'af sock_addr
    val getNREAD     : ('af, 'st) sock -> int
    val getATMARK    : ('af, active stream) sock -> bool
*)
    end

    type sock_desc = int
    fun sockDesc (s: ('af, 'st) sock) : sock_desc = #fd s
    fun sameDesc (s1: sock_desc, s2: sock_desc) : bool = s1 = s2

    fun select { rds : sock_desc list,
                 wrs : sock_desc list,
                 exs : sock_desc list,
                 timeout : Time.time option
               } : { rds : sock_desc list,
                     wrs : sock_desc list,
                     exs : sock_desc list
                   } =
        let val t = case timeout of NONE => ~1.0
                                  | SOME t => Time.toReal t
            val (rds, wrs, exs) = prim("sml_sock_select", (getCtx(),rds,wrs,exs,t))
        in {rds=List.rev rds, wrs=List.rev wrs, exs=List.rev exs}
        end

    fun ioDesc (s: ('af, 'st) sock) : OS.IO.iodesc =
        prim("id", #fd s)

    type out_flags = {don't_route : bool, oob : bool}
    type in_flags = {peek : bool, oob : bool}

    fun sendVec ({fd,...} : ('af, active stream) sock,
                 slc : Word8VectorSlice.slice) : int =
        let val (v,i,n) = Word8VectorSlice.base slc
            val ret = prim("sml_sock_sendvec", (fd,v,i,n))
        in maybe_failure "sendVec" ret
         ; ret
        end

    (* Word8ArraySlices are represented the same as Word8VectorSlices *)
    fun sendArr ({fd,...} : ('af, active stream) sock,
                 slc : Word8ArraySlice.slice) : int =
        let val (v,i,n) = Word8ArraySlice.base slc
            val ret = prim("sml_sock_sendvec", (fd,v,i,n))
        in maybe_failure "sendArr" ret
         ; ret
        end

    fun recvVec ({fd,...} : ('af, active stream) sock,
                 i : int) : Word8Vector.vector =
        prim("sml_sock_recvvec",(getCtx(),fd,i))
        handle Overflow => failure "recvVec"

    fun close ({fd,...} : ('af, 'st) sock) : unit =
        prim("@close", fd)

    datatype shutdown_mode = NO_RECVS
                           | NO_SENDS
                           | NO_RECVS_OR_SENDS

    fun shutdown (s: ('af, 'mode stream) sock,
                  sm: shutdown_mode) : unit =
        let val ret = prim("@shutdown",
                           (#fd s,
                            case sm of
                                NO_RECVS => SHUT_RD
                              | NO_SENDS => SHUT_WR
                              | NO_RECVS_OR_SENDS => SHUT_RDWR))
        in maybe_failure "Socket.shutdown" ret
        end

    fun sameAddr (a1: 'af sock_addr, a2: 'af sock_addr) : bool =
        a1 = a2

    fun familyOfAddr (sa: 'af sock_addr) : AF.addr_family =
        case sa of
            Inet_sa _ => AF_INET
          | Unix_sa _ => AF_UNIX

    fun bind ({fd,af} : ('af, 'st) sock, a: 'af sock_addr) : unit =
        case (af, a) of
            (Inet_af, Inet_sa {addr,port}) =>
            let val ret : int = prim("sml_sock_bind_inet", (fd,addr,port))
            in maybe_failure "bind" ret
            end
          | (Unix_af, Unix_sa {name}) =>
            let val ret : int = prim("sml_sock_bind_unix", (fd,name))
            in maybe_failure "bind" ret
            end
          | _ => raise Fail "Socket.impossible"

    fun listen ({fd,...} : ('af, passive stream) sock, i: int) : unit =
        let val ret : int = prim("sml_sock_listen", (fd,i))
        in maybe_failure "listen" ret
        end

    fun accept ({fd,af} : ('af, passive stream) sock)
        : ('af, active stream) sock * 'af sock_addr =
        case af of
            Inet_af =>
            let val (fd', addr, port) = prim("sml_sock_accept_inet",(getCtx(),fd))
                                        handle Overflow => failure "accept"
            in ({fd=fd',af=af}, Inet_sa{addr=addr,port=port})
            end
          | Unix_af =>
            let val (fd', name) = prim("sml_sock_accept_unix",(getCtx(),fd))
                                  handle Overflow => failure "accept"
            in ({fd=fd',af=af}, Unix_sa{name=name})
            end

  end

  structure INetSock = struct
    datatype inet = INET
    type 'st sock = (inet, 'st) Socket.sock
    type 'm stream_sock = 'm Socket.stream sock
    type sock_addr = inet Socket.sock_addr
    val inetAF = Socket.AF_INET
    
    fun toAddr (ia:int, port:int) =
        Socket.Inet_sa {addr=ia,port=port}
    
    fun any (p:int) : sock_addr =
        toAddr (Socket.INADDR_ANY,p)

    fun fromAddr (Socket.Inet_sa{addr,port}) = (addr,port)
      | fromAddr _ = raise Fail "INetSock.fromAddr: impossible"

    structure TCP = struct
    fun socket () : 'm stream_sock =
        let val res = prim("sml_sock_socket", (Socket.AF_INET,Socket.SOCK_STREAM))
        in maybe_failure "socket" res
         ; {fd=res,af=Socket.Inet_af}
        end
    end
  end

  structure All :>
            sig
              structure Socket : SOCKET
              structure INetSock :
                        sig
                          type inet
                          type 'st sock = (inet,'st) Socket.sock
                          type 'm stream_sock = 'm Socket.stream sock
                          type sock_addr = inet Socket.sock_addr

                          val inetAF : Socket.AF.addr_family
                          val toAddr : NetHostDB.in_addr * int -> sock_addr
                          val fromAddr : sock_addr -> NetHostDB.in_addr * int
                          val any : int -> sock_addr

                          structure TCP :
                                    sig
                                      val socket : unit -> 'm stream_sock
                                    end
                        end
            end =
  struct
  structure Socket = Socket
  structure INetSock = INetSock
  end

in
  structure Socket = All.Socket
  structure INetSock = All.INetSock
end
