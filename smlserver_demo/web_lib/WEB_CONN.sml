signature WEB_CONN = sig
  type set
  val returnHtml         : int * string -> unit
  val returnXhtml        : int * string -> unit
  val return             : string -> unit
  val returnFile         : int * string * string -> unit
  val write              : string -> unit
  val returnRedirect     : string -> unit
  val returnRedirectWithCode     : int * string -> unit
  val setMimeType        : string -> unit
  val getQuery           : unit -> set option
  val formvar            : string -> string option
  val formvarAll         : string -> string list
  val storeMultiformData : string * string -> unit
  val headers            : unit -> set 
  val host               : unit -> string 
  val location           : unit -> string 
  val peer               : unit -> string 
  val scheme             : unit -> string
  val port               : unit -> int
  val redirect           : string -> unit
  val server             : unit -> string
  val url                : unit -> string list
  val method             : unit -> string
  val contentLength      : unit -> int
  val hasConnection      : unit -> bool
  val add_headers        : (string * string) -> unit
end

(*
 [set] abstract type identical to Web.Set.set.

 [returnHtml (sc,s)] sends HTML string s with status code sc and
 mime-type text/html to client, including HTTP headers and
 Cache-Control header set to no-cache. May raise MissingConnection.

 [returnXHtml (sc,s)] sends XHTML string s with status code sc and
 mime-type application/xhtml+xml to client, including HTTP headers and
 Cache-Control header set to must-revalidate. May raise
 MissingConnection.

 [return s] sends HTML string s with status code 200 to 
 client, including HTTP headers. May raise MissingConnection.

 [returnFile (sc,mt,f)] sends file f with status code sc to 
 client, including HTTP headers. The mime type is mt. Raises
 MissingConnection if the execution is not associated with a
 connection. Raises Fail(msg) if the file cannot be opened for
 reading.

 [write s] sends string s to client, excluding HTTP headers. 
 May raise MissingConnection.

 [returnRedirect loc] sends redirection HTTP response to 
 client (status code 302), with information that the client 
 should request location loc. May raise MissingConnection.

 [getQuery()] constructs and returns a set representing the 
 query data associated with the connection. It reads the POST 
 content or the query string. The POST content takes 
 precedence over the query string.

 [formvar k] returns the first query data associated with the 
 key k; the function returns NONE if no query data is present 
 for the argument key k.

 [formvarAll k] returns all values associated with key k in 
 the query data; the function returns the empty list if no 
 query data is present for the argument key k.

 [storeMultiformData (fv,filename)] stores the uploaded file
 represented by formvariable fv in file filename. Raises Fail 
 if some error happens (e.g., filename can't be opened, fv 
 does not exists or fv is not an uploaded file.

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
 i.e., makes it appear that the user requested a different 
 URL and then run that request. This form of redirect does 
 not require the running of an additional thread.

 [server()] returns the name of the server associated with 
 the connection.

 [url()] return the url (relative to server-root) associated 
 with the request.

 [hasConnection()] returns true if a connection is available.
 Returns false otherwise. For the execution of init scripts
 and scheduled scripts, no connection is available. This 
 function may be used to protect execution of code that 
 requires a connection (e.g., execution of library code).
  
 [add_headers (key,value)] adds key:value to the http header
*)
