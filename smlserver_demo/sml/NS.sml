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

    structure Conn :
      sig
	val returnHtml : int * string -> status

	val returnRedirect : string -> status

	(* The 'getQuery' function constructs and returns 
	 * a set representing the query data associated 
	 * with the connection. It reads the POST content 
	 * or the query string. The POST content takes 
	 * precedence over the query string. Note that 
	 * you must not call Set.free on the result of 
	 * this function. *)
	val getQuery : unit -> Set.set option

	(* The function 'formvar' returns the query data 
	 * associated with the connection and the argument 
	 * key; the function returns NONE if no query data 
	 * is present for the argument key. *)
	val formvar : string -> string option

	(* The function 'headers' returns, as a set, 
	 * the headers associated with the connection. *)
	val headers : unit -> Set.set 

	(* Returns the server hostname associated with 
	 * the connection. *)
	val host : unit -> string 

	(* The 'location' function returns the HTTP 
	 * location associated with the connection. For 
	 * example: http://www.avalon.com:81. Multiple 
	 * communications drivers can be loaded into a 
	 * single server. This means a server may have 
	 * more than one location. For example, if the 
	 * nsssl module is loaded and bound to port 8000 
	 * and the nssock module is loaded and bound to 
	 * port 9000, the server would have the 
	 * following two locations:
	 *      http://www.avalon.com:9000
	 *      https://www.avalon.com:8000
	 * For this reason it is important to use the 
	 * location function to determine the driver 
	 * location at run time. *)
	val location : unit -> string 

	(* The 'peer' function returns the name of the 
	 * peer associated with the connection. The peer 
	 * address is determined by the communications 
	 * driver in use by the connection. Typically,
	 * it is a dotted IP address, for example, 
	 * 199.221.53.205, but this is not guaranteed. *)
	val peer : unit -> string 

	(* The function 'peerPort' returns the port from 
	 * which the peer is connected. *)
	val peerPort : unit -> int

	(* The 'port' function returns the server port 
	 * number associated with the connection. *)
	val port : unit -> int

	(* The function 'redirect' performs an internal 
	 * redirect, i.e., make it appear that the user 
	 * requested a different URL and then run that 
	 * request. This doesn't require an additional 
	 * thread. *)
	val redirect : string -> status

	(* The `server' function returns the name of the 
	 * server associated with the connection. *)
	val server : unit -> string

	(* Return the url (relative to server-root) 
	 * associated with the request. *)
	val url : unit -> string
      end

    structure Cache :
      sig
	type cache
	  
	(* Find a cache, given a cache name. Returns 
	 * NONE if cache does not exist. *)
        val find : string -> cache option

	(* Create a cache, given a cache name and a 
	 * timeout value in seconds. *)
        val create : string * int -> cache

	(* Create a cache, given a cache name and a 
	 * maximum cache size in bytes. *)
	val createSz : string * int -> cache  

	(* Deletes all entries in cache. *)
	val flush : cache -> unit

	(* Associate a key with a value in the cache; 
	 * Overwrites existing entry in cache if 
	 * entry is present, in which case the 
	 * function returns `false'. If no previous 
	 * entry for the key is present in the cache, 
	 * the function returns `true'. *)
	val set : cache * string * string -> bool

	(* Returns value associated with key in cache; 
	 * returns NONE if key does not exist in 
	 * cache. *)
	val get : cache * string -> string option
      end

    structure Info :
      sig
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
	  {sectionName: string, key: string} 
	  -> string option

	(* The case-sensitive counterpart of 
	 * configGetValue. *)
	val configGetValueExact : 
	  {sectionName: string, key: string} 
	  -> string option

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
      end
	
    (* Return html string to browser, including 
     * HTTP headers. *)    
    val return : string -> status    
           
    (* Write string to browser. *)
    val write : string -> status

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

    structure Quot :
      sig
	(* Return HTML frag list to browser, 
	 * including HTTP headers; used with 
	 * quotation support. *)
	val return : string frag list -> status

	(* Write frag list to browser; used with 
	 * quotation support. *)
	val write : string frag list -> status  
      
	val flatten : string frag list -> string

        val ^^ : 'a frag list * 'a frag list -> 'a frag list
      end

    structure Mail : 
      sig
	val sendmail : 
	  {to: string list, cc: string list, 
	   bcc: string list,
	   from: string, subject: string, body: string,
	   extra_headers: string list} -> unit 
	val send : 
	  {to: string, from: string, 
	   subject: string, body: string} -> unit
      end

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
    structure DbOra : NS_DB where type status = status
    structure DbPg  : NS_DB where type status = status
  end
