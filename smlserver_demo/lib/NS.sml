signature NS =
  sig
    eqtype LogSeverity
    
    (* Something interesting occurred. *)
    val Notice : LogSeverity     

    (* Maybe something bad occurred. *)
    val Warning : LogSeverity

    (* Something bad occurred. *)      
    val Error : LogSeverity

    (* Something extremely bad occurred. The server 
     * will shut down after logging this message. *)
    val Fatal : LogSeverity

    (* Something occurred that implies there 
     * is a bug in your code. *)
    val Bug : LogSeverity
				
    (* If the server is in Debug mode, the message 
     * is printed. Debug mode is specified in 
     * the [ns/parameters] section of the configuration 
     * file. If the server is not in debug mode, the 
     * message is not printed. *)
    val Debug : LogSeverity

    (* Write a string to the log file. *)
    val log : LogSeverity * string -> unit

    eqtype status
    val OK : status
    val ERROR : status
    val END_DATA : status

    structure Set    : NS_SET
    structure Conn   : NS_CONN where type status = status 
	                         and type set = Set.set
    structure Cookie : NS_COOKIE
    structure Cache  : NS_CACHE
    structure Info   : NS_INFO
    structure Mail   : NS_MAIL

    (* Quotation support. *)
    type quot = Quot.quot

    (* Return html string to browser with status code 200, 
     * including HTTP headers. *)    
    val return : quot -> status    
           
    (* Write string to browser. *)
    val write : quot -> status

    (* Write HTTP headers to browser. *)
    val returnHeaders : unit -> unit

    (* Write a redirection HTTP response. *)
    val returnRedirect : string -> status

    (* Guess the Mime type based on the filename 
     * extension. Case is ignored. The return value 
     * is of the form: "text/html". *)
    val getMimeType : string -> string

    (* Converts a numeric IP address into a host 
     * name. If no name can be found, NONE is 
     * returned. Because the response time of the 
     * Domain Name Service can be slow, this 
     * function may significantly delay the 
     * response to a client. *)
    val getHostByAddr : string -> string option 

    (* Returns an encoded version of the argument 
     * as URL query data. All characters except the 
     * alphanumerics are encoded as specified in 
     * RFC1738, Uniform Resource Locators. This 
     * function can be used to append arguments to 
     * a URL as query data following a `?'. *)
    val encodeUrl : string -> string

    (* Decodes data that were encoded as URL query 
     * data. The decoded data is returned. *)
    val decodeUrl : string -> string

    val buildUrl : string -> (string * string) list -> string

    (* Fetch a remote URL; connects AOLserver to 
     * another HTTP Web server and requests the 
     * specified URL. The URL must be fully 
     * qualified. Currently, the function cannot 
     * handle redirects or requests for any protocol 
     * except HTTP. Returns NONE if no page is 
     * found. *)
    val fetchUrl : string -> string option

    val exit : unit -> 'a

    (* Creating the two supported database interfaces *)
    structure DbOra   : NS_DB where type status = status and type set = Set.set
    structure DbPg    : NS_DB where type status = status and type set = Set.set
    structure DbMySQL : NS_DB where type status = status and type set = Set.set
  end
