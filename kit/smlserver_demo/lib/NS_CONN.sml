signature NS_CONN =
  sig

    eqtype status
    type set

    (* [returnHtml (sc,s)] sends HTML string s with 
     * status code sc to client, including HTTP 
     * headers. Returns Ns.OK on success and Ns.ERROR
     * on failure. *)
    val returnHtml : int * string -> status

    (* [return s] sends HTML string s with status
     * code 200 to client, including HTTP headers. 
     * Returns Ns.OK on success and Ns.ERROR on 
     * failure. *)    
    val return : string -> status

    (* [write s] sends string s to client, excluding
     * HTTP headers. Returns Ns.OK on success and 
     * Ns.ERROR on failure. *)
    val write : string -> status

    (* [returnRedirect loc] sends redirection HTTP 
     * response to client, with information that
     * the client should request location loc. 
     * Returns Ns.OK on success and Ns.ERROR on 
     * failure. *)
    val returnRedirect : string -> status

    (* [getQuery()] constructs and returns a set 
     * representing the query data associated with 
     * the connection. It reads the POST content 
     * or the query string. The POST content takes 
     * precedence over the query string. *)
    val getQuery : unit -> set option

    (* [formvar k] returns the query data associated 
     * with the connection and the key k; the 
     * function returns NONE if no query data is 
     * present for the argument key k. *)
    val formvar : string -> string option

    (* [formvarAll k] returns all values associated 
     * with key k in the query data; the function 
     * returns the empty list if no query data is 
     * present for the argument key k. *)
    val formvarAll : string -> string list

    (* [headers()] returns, as a set, the HTTP 
     * headers associated with the connection. *)
    val headers : unit -> set 

    (* [host()] returns the server hostname 
     * associated with the connection. *)
    val host : unit -> string 

    (* [location()] returns the HTTP location 
     * associated with the connection. For 
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

    (* [peer()] returns the name of the peer 
     * associated with the connection. The peer 
     * address is determined by the communications 
     * driver in use by the connection. Typically,
     * it is a dotted IP address, for example, 
     * 199.221.53.205, but this is not guaranteed. *)
    val peer : unit -> string 

    (* [peerPort()] returns the port from which the 
     * peer is connected. *)
    val peerPort : unit -> int

    (* [port()] returns the server port number 
     * associated with the connection. *)
    val port : unit -> int

    (* [redirect f] performs an internal redirect, 
     * to the file f; i.e., make it appear that the 
     * user requested a different URL and then run 
     * that request. This form of redirect doesn't 
     * require the running of an additional 
     * thread. *)
    val redirect : string -> status

    (* [server()] returns the name of the server 
     * associated with the connection. *)
    val server : unit -> string

    (* [url()] return the url (relative to server-
     * root) associated with the request. *)
    val url : unit -> string
  end






