signature SMLSERVER = sig

  val log      : string -> unit
  val exit     : unit -> 'a

  val fetchUrl : string -> string option
  val hostAddr : string -> string option

  val headers  : unit -> (string * string) list
  val mimeType : string -> string

  structure Unsafe  : SMLSERVER_UNSAFE
  structure Cookie  : SMLSERVER_COOKIE
  structure Info    : SMLSERVER_INFO
  structure Cache   : SMLSERVER_CACHE
  structure Mail    : SMLSERVER_MAIL
  structure DbOra   : SMLSERVER_DB
  structure DbPg    : SMLSERVER_DB
  structure DbMySQL : SMLSERVER_DB
end

(*
 [log s] write the string s to the log file.

 [exit()] terminates the script by raising the exception
 Interrupt, which is silently caught by the SMLserver module 
 (other uncaught exceptions are logged in the server.log 
 file).

 [fetchUrl u] fetches a remote URL u; connects the Web 
 server to another HTTP Web server and requests the 
 specified URL. The URL must be fully qualified. Currently, 
 the function cannot handle redirects or requests for any 
 protocol except HTTP. Returns NONE if no page is found.

 [hostAddr ip] converts a numeric IP address ip into a host
 name. If no name can be found, NONE is returned. Because 
 the response time of the Domain Name Service can be slow, 
 this function may significantly delay the response to a 
 client.

 [headers()] returns, as a list of string pairs, the HTTP 
 headers associated with the request.

 [mimeType f] guesses the Mime type based on the extension 
 of the filename f. Case is ignored. The return value is of 
 the form "text/html".
*)
