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
	
	(* Return html string to browser, including 
	 * HTTP headers. *)
	val returnHtml : int * string -> status

	(* Return html string to browser with status code 200, 
	 * including HTTP headers. *)    
	val return : string -> status

	(* Return string to browser. *)
	val write : string -> status

	(* Return redirection HTTP response to browser. *)
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

	(* As formvar, except that all values associated 
         * with key is returned; the function returns the
         * empty list if no query data is present for the
         * argument key. *)
	val formvarAll : string -> string list

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

    structure Cookie :
      sig
	exception CookieError of string
	type cookiedata = 
	  {name   : string, 
	   value  : string, 
	   expiry : Date.date option, 
	   domain : string option, 
	   path   : string option, 
	   secure : bool}

      (* [allCookies] is a list [(n1,v1), (n2,v2), ..., (nm,vm)] of all 
         the name=value pairs of defined cookies. *)
	val allCookies     : (string * string) list

      (* [getCookie cn] returns SOME(value) where value is the 
         cn=value string for the cookie cn, if any; otherwise 
	 returns NONE. *)
	val getCookie      : string -> (string * string) option

      (* [getCookieValue cn] returns SOME(v) where v is the 
         value associated with the cookie cn, if any; 
	 otherwise returns NONE. *)
	val getCookieValue : string -> string option

      (* [setCookie { name, value, expiry, domain, path, secure }]
         returns a string which (when transmitted to a browser as 
	 part of the HTTP response header) sets a cookie with the 
	 given name, value, expiry date, domain, path, and 
	 security. *)
	val setCookie    : cookiedata -> string

      (* [setCookies ckds] returns a string which (when transmitted 
         to a browser as part of the HTTP response header) sets the 
         specified cookies. *)
	val setCookies   : cookiedata list -> string

      (* [deleteCookie { name, path }] returns a string which (when
         transmitted to a browser as part of the HTTP response header)
         deletes the specified cookie by setting its expiry to some time in
	 the past. *)
	val deleteCookie : { name : string, path : string option } -> string
      end

    structure Cache :
      sig
	type cache

	(* Create a cache, given a cache name and a 
	 * timeout value in seconds. *)
        val createTm : string * int -> cache

	(* Create a cache, given a cache name and a 
	 * maximum cache size in bytes. *)
	val createSz : string * int -> cache  
	  
	(* Find a cache, given a cache name. Returns 
	 * NONE if cache does not exist. *)
        val find : string -> cache option

	(* findTm (cn,t) : As find, except that the cache 
           with name cn is created if it does not already
           exist. If the cache is created then t is used 
           as timeout value in seconds. *)
	val findTm : string * int -> cache

	(* findSz (cn,s) : As find, except that the cache 
           with name cn is created if it does not already
           exist. If the cache is created then s is used 
           as size in bytes. *)
        val findSz : string * int -> cache

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

	(* cacheForAwhile (f,cn,t): given a function f 
           that maps a string to a string, a cache name cn
           and a timeout value in seconds t, a new function
           f' is returned. f' is equal to f except that the
           results are cached and only recalculated when the
           cached results are older than the timeout value.
           This can for instance be used to cache fetched
           HTML pages from the Internet. The timestamp is
           not renewed when items are accessed. This is not
           what you get with createTm, and is therefore
	   simulated explicitly (i.e., this is a little 
           slower than cacheWhileUsed). *)
        val cacheForAwhile : 
          (string -> string) * string * int -> string -> string

	 (* cacheWhileUsed (f,cn,t): as casheForAwhile, except
            that the timestamp is renewed at each access. An item
            is removed from the cache if t seconds have passed after
            the last access. This is what you get with createTm. *)
        val cacheWhileUsed : 
          (string -> string) * string * int -> string -> string
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
      end

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

    (* A global random generator *)
    val randomGenerator : Random.generator

    (* Creating the two supported database interfaces *)
    structure DbOra   : NS_DB where type status = status and type set = Set.set
    structure DbPg    : NS_DB where type status = status and type set = Set.set
    structure DbMySQL : NS_DB where type status = status and type set = Set.set
  end
