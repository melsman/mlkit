signature NS_CONN =
  sig

    eqtype status
    type set

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
    val getQuery : unit -> set option

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
    val headers : unit -> set 

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






