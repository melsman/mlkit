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
    val getConn : unit -> conn  (* get the current connection - a C structure *)
      
    eqtype status
    val OK : status
    val ERROR : status
    val END_DATA : status

    type set

    structure Conn :
      sig
	val returnHtml : conn * int * string -> status

	val returnRedirect : conn * string -> status

	(* The Ns_ConnGetQuery function constructs and returns an
	   Ns_Set structure representing the query data associated
	   with the connection. It reads the POST content or the query
	   string. The POST content takes precedence over the query
	   string.

           Note that you must not call Ns_SetFree on the result of
	   this function. *)

	val getQuery : conn -> set option
      end

    structure Set :
      sig
	val get : set * string -> string option
	val getOpt : set * string * string -> string
	val put : set * string * string -> unit (* Add a field to an Ns_Set *)
	val free : set -> unit                  (* Free memory used by an Ns_Set *)
	val create : string -> set              (* Create a new Ns_Set *)
	val size : set -> int                   (* Return the current size of an Ns_Set *)
	val unique : set * string -> bool       (* Check if a key in an Ns_Set is unique, 
						 * case sensitive *)
      end

    type db
    type poolname = string
    structure Db :
      sig
	val poolGetHandle : poolname -> db
	val poolPutHandle : db -> unit
	val dmlDb : db * string -> status
	val dml : string -> status
	val select : db * string -> set
	val getRow : db * set -> status
	val foldDb : db * ((string->string)*'a->'a) * 'a * string -> 'a
	val fold : ((string->string)*'a->'a) * 'a * string -> 'a
      end
	
    val return : string -> status               (* Return html string to browser,
						 * including HTTP headers. *)
    val write : string -> status                (* Write string to browser. *)
    val returnHeaders : unit -> unit            (* Write HTTP headers to browser. *)
    val returnRedirect : string -> status       (* Write a redirection HTTP response. *)

    val returnQuot : string frag list -> status (* Return HTML frag list to browser,
						 * including HTTP headers. *)
    val writeQuot : string frag list -> status  (* Write frag list to browser. *) 
      
    val pageRoot : unit -> string
  end
