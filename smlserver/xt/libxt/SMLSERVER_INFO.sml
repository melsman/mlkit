signature SMLSERVER_INFO = sig

  val hostname : unit -> string
  val host     : unit -> string 
  val port     : unit -> int

  val pageRoot : unit -> string
  val location : unit -> string 
  val url      : unit -> string
end

(*
 [hostname()] returns the host name of the machine.

 [host()] returns the IP address associated with the server.

 [port()] returns the port number associated with the server.

 [pageRoot()] returns the directory for which the server 
 serves pages.

 [location()] returns the HTTP location associated with the
 server. For example: http://www.avalon.com:81.

 [url()] return the url (relative to pageRoot) associated 
 with the request.
*)
