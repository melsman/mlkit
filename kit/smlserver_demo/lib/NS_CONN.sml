signature NS_CONN = sig
  eqtype status
  type set
  val returnHtml     : int * string -> status
  val return         : string -> status
  val returnFile     : int * string * string -> status
  val write          : string -> status
  val returnRedirect : string -> status
  val getQuery       : unit -> set option
  val formvar        : string -> string option
  val formvarAll     : string -> string list
  val headers        : unit -> set 
  val host           : unit -> string 
  val location       : unit -> string 
  val peer           : unit -> string 
  val peerPort       : unit -> int
  val port           : unit -> int
  val redirect       : string -> status
  val server         : unit -> string
  val url            : unit -> string
end

(*
 [status] abstract type identical to Ns.status.

 [set] abstract type identical to Ns.Set.set.

 [returnHtml (sc,s)] sends HTML string s with status code sc 
 to client, including HTTP headers. Returns Ns.OK on success 
 and Ns.ERROR on failure.

 [return s] sends HTML string s with status code 200 to 
 client, including HTTP headers. Returns Ns.OK on success 
 and Ns.ERROR on failure.

 [returnFile (sc,mt,f)] sends file f with status code sc to 
 client, including HTTP headers. The mime type is mt. 
 Returns Ns.OK on success and Ns.ERROR on failure.

 [write s] sends string s to client, excluding HTTP headers. 
 Returns Ns.OK on success and Ns.ERROR on failure.

 [returnRedirect loc] sends redirection HTTP response to 
 client, with information that the client should request 
 location loc. Returns Ns.OK on success and Ns.ERROR on 
 failure.

 [getQuery()] constructs and returns a set representing the 
 query data associated with the connection. It reads the POST 
 content or the query string. The POST content takes 
 precedence over the query string.

 [formvar k] returns the query data associated with the 
 connection and the key k; the function returns NONE if no 
 query data is present for the argument key k.

 [formvarAll k] returns all values associated with key k in 
 the query data; the function returns the empty list if no 
 query data is present for the argument key k.

 [headers()] returns, as a set, the HTTP headers associated 
 with the connection.

 [host()] returns the server hostname associated with the 
 connection.
 
 [location()] returns the HTTP location associated with the 
 connection. For example: http://www.avalon.com:81. Multiple 
 communications drivers can be loaded into a single server. 
 This means a server may have more than one location. For 
 example, if the nsssl module is loaded and bound to port 
 8000 and the nssock module is loaded and bound to port 9000, 
 the server would have the following two locations:
      http://www.avalon.com:9000
      https://www.avalon.com:8000
 For this reason it is important to use the location function
 to determine the driver location at run time.

 [peer()] returns the name of the peer associated with the 
 connection. The peer address is determined by the 
 communications driver in use by the connection. Typically,
 it is a dotted IP address, for example, 199.221.53.205, but 
 this is not guaranteed.

 [peerPort()] returns the port from which the peer is 
 connected.

 [port()] returns the server port number associated with the 
 connection.

 [redirect f] performs an internal redirect, to the file f; 
 i.e., make it appear that the user requested a different URL 
 and then run that request. This form of redirect does not 
 require the running of an additional thread.

 [server()] returns the name of the server associated with 
 the connection.

 [url()] return the url (relative to server-root) associated 
 with the request.
*)