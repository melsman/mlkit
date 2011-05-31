signature SMLSERVER = sig

  val log       : string -> unit

(*
  type exitId
  val atExit    : (unit -> unit) -> exitId
  val exitUnreg : exitId -> unit
*)
  val exit      : unit -> 'a

  val fetchUrl  : string -> string option
  val hostAddr  : string -> string option

  val headers   : unit -> (string * string) list
  val mimeType  : string -> string

  structure Unsafe  : SMLSERVER_UNSAFE
  structure Cookie  : SMLSERVER_COOKIE
  structure Info    : SMLSERVER_INFO
  structure Cache   : SMLSERVER_CACHE
  structure Mail    : SMLSERVER_MAIL
  structure DbOra   : SMLSERVER_DB
  structure DbPg    : SMLSERVER_DB
  structure DbMySQL : SMLSERVER_DB
  structure Form    : SMLSERVER_FORM
      sharing type Form.var = Unsafe.Form.var
end

(*
 [log s] write the string s to the log file.

 [exit()] terminates the script by first executing 
 registered ``at exit'' functions.

 [atExit f] registers the function f to be executed upon
 calls to exit(); returns a unique id, which may be given 
 to the exitUnreg function to unregister the execution of
 the function upon exits.

 [exitUnreg eid] unregisters the execution of the ``at
 exit'' function identified by eid.

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
