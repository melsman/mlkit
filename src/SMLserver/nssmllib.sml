signature NS =
  sig
    eqtype LogSeverity
    val Notice : LogSeverity     (* Something interesting occurred. *)
    val Warning : LogSeverity    (* Maybe something bad occurred. *)
    val Error : LogSeverity      (* Something bad occurred. *)
    val Fatal : LogSeverity      (* Something extremely bad occurred. The server 
				  * will shut down after logging this message. *)
    val Bug : LogSeverity        (* Something occurred that implies there 
				  * is a bug in your code. *)
    val Debug : LogSeverity      (* If the server is in Debug mode, the message 
				  * is printed. Debug mode is specified in 
				  * the [ns/parameters] section of the configuration 
				  * file. If the server is not in debug mode, the 
				  * message is not printed. *)

    val log : LogSeverity * string -> unit

    type conn
    val get_conn : unit -> conn  (* get the current connection - a C structure *)
      
    eqtype status
    val OK : status
    val ERROR : status

    structure Conn :
      sig
	val returnHtml : conn * int * string -> status
      end

    val return : string -> status

  end

structure Ns : NS =
  struct
    type LogSeverity = int
    val Notice = 0
    val Warning = 1
    val Error = 2
    val Fatal = 3
    val Bug = 4
    val Debug = 5

    fun log (ls: LogSeverity, s: string) : unit =
      prim("nssml_log", "nssml_log", (ls, s))

    type conn = int  (* we could use Ns_TclGetConn!! *)
    fun get_conn () : conn = prim("__get_conn", "__get_conn", ())

    type status = int        (* see nsthread.h *)
    val OK = 0
    val ERROR = ~1

    structure Conn =
      struct
	fun returnHtml(c: conn, status: int, s: string) : status =
	  prim("nssml_ConnReturnHtml", "nssml_ConnReturnHtml", (c,status,s))
      end

    fun return (s : string) : status =
      returnHtml(get_conn(),200,s)

  end