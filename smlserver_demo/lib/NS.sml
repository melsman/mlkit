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

    structure Set : NS_SET

    structure Conn : NS_CONN where type status = status and type set = Set.set

    structure Cookie : NS_COOKIE

    structure Cache : NS_CACHE

    structure Info : NS_INFO
(*      sig 
	(* Return full path name of the configuration 
	 * file in use. *)
	val configFile : unit -> string           

	(* Return the value for the given key in the 
	 * section named sectionName. If either the 
	 * section does not exist or the key does not 
	 * exist in the section, the function returns 
	 * NONE. If multiple keys of the same name are 
	 * in the named section (for example, the 
	 * multiple Load lines of the Modules section), 
	 * this function returns only the first 
	 * matching entry. The section names must match 
	 * exactly, but the key will be matched case-
	 * insensitively. *)
	val configGetValue :
	  {sectionName: string, key: string} -> string option

	(* The case-sensitive counterpart of 
	 * configGetValue. *)
	val configGetValueExact : 
	  {sectionName: string, key: string} -> string option

	(* Return the name of the error log. *)
	val errorLog : unit -> string

	(* Return directory where AOLserver is 
	 * installed. *)
	val homePath : unit -> string

	(* Return the hostname that AOLserver thinks 
	 * it's running on, as specified in the 
	 * configuration file. *)
	val hostname : unit -> string

	(* Return pid (process id) of AOLserver. *)
	val pid : unit -> int

	(* Return AOLserver version string. *)
	val serverVersion : unit -> string

	(* Return how long, in seconds, AOLserver has 
	 * been running. *)
	val uptime : unit -> int

	(* Return path name of the AOLserver pages 
	 * directory for a server. *)
	val pageRoot : unit -> string
      end   *)

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

    structure Mail : NS_MAIL

    (* Fetch a remote URL; connects AOLserver to 
     * another HTTP Web server and requests the 
     * specified URL. The URL must be fully 
     * qualified. Currently, the function cannot 
     * handle redirects or requests for any protocol 
     * except HTTP. Returns NONE if no page is 
     * found. *)
    val fetchUrl : string -> string option

    val exit : unit -> 'a

    (* A global random generator *)
    val randomGenerator : Random.generator

    (* Creating the two supported database interfaces *)
    structure DbOra   : NS_DB where type status = status and type set = Set.set
    structure DbPg    : NS_DB where type status = status and type set = Set.set
    structure DbMySQL : NS_DB where type status = status and type set = Set.set
  end
