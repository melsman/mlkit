signature WEB = sig
  include WEB_LOG
  exception MissingConnection
  exception Forbidden

  type quot = Quot.quot

  val return         : quot -> unit
  val write          : quot -> unit
  val returnRedirect : string -> unit
  val encodeUrl      : string -> string
  val decodeUrl      : string -> string
  val returnFileMime : string -> string -> unit
  val returnFile     : string -> unit
  val fetchUrl       : string -> string option
  val fetchUrlTime   : int -> string -> string option
  val buildUrl       : string -> (string * string) list -> string

  val schedule       : string -> string option -> Date.date ->
                       Time.time -> unit
  val deSchedule     : string -> unit
  val scheduleScript : string -> string option -> int -> unit
  val scheduleDaily  : string -> string option ->
                       {hour:int, minute:int} -> unit
  val scheduleWeekly : string -> string option ->
                       {day:Date.weekday, hour:int, minute:int} ->
                       unit

  val exit           : unit -> 'a

  structure Set      : WEB_SET
  structure Conn     : WEB_CONN where type set = Set.set 
  structure Cookie   : WEB_COOKIE
  structure Info     : WEB_INFO
  structure Mail     : WEB_MAIL
  structure Cache    : WEB_CACHE
  structure Mime     : WEB_MIME
  structure LowMail  : WEB_LOW_MAIL 
  structure DbOraBackend : WEB_DB_BACKEND where
                           type 'a Type = 'a Info.Type.Type 
  structure DbMySqlBackend : WEB_DB_BACKEND where
                             type 'a Type = 'a Info.Type.Type 
  structure DbPostgreSQLBackend : WEB_DB_BACKEND where
                                  type 'a Type = 'a Info.Type.Type 

  structure WebDynlib : WEB_DYNLIB

  structure XMLrpc : XMLRPC
end

(*
 [MissingConnection] exception raised by functions that cannot be
 called when no connection is present (e.g., at initialization time).

 [Forbidden] exception raised by some functions on illegal input.

 [quot] type of quotations.

 [return s] sends HTML string s with status code 200 to 
 client, including HTTP headers. May raise MissingConnection.

 [write s] sends string s to client, excluding HTTP headers. 
 May raise MissingConnection.

 [returnRedirect loc] sends redirection HTTP response to 
 client (status code 302), with information that the client 
 should request location loc. May raise MissingConnection.

 [encodeUrl s] returns an encoded version of the argument s as
 URL query data. All characters except the alphanumerics are
 encoded as specified in RFC1738, Uniform Resource Locators.
 This function can be used to append arguments to a URL as
 query data following a `?'.

 [decodeUrl s] decodes data s that was encoded as URL query
 data. The decoded data is returned.

 [returnFileMime mimetype file] returns the entire contents of the
 given file to the client. In addition to setting the HTTP status
 response line to 200 and the Content-Type header from the given
 parameter, the function also uses the stat system call to generate
 the appropriate Last-Modified and Content-Length headers. May raise
 MissingConnection or Fail(msg) if file cannot be accessed.

 [returnFile file] as returnFileMime, but gets the
 Content-Type (mimetype) argument from calling the function
 Web.Mime.getMime with the given file as parameter.

 [fetchUrl u] fetches a remote URL u; connects the Web server
 to another HTTP Web server and requests the specified URL.
 The URL must be fully qualified. Currently, the function
 cannot handle redirects or requests for any protocol except
 HTTP. Returns NONE if no page is found.

 [fetchUrlTime u] as fetchUrl but with a specified timeout in 
 seconds.

 [buildUrl u l] constructs a link to the URL u with the form
 variable pairs l appended to u?, delimited by &, and with the
 form values URL encoded.

 [schedule s serv d t] schedule a script s to be executed on server
 serv on date d at time t. If serv is NONE localhost is used as
 server.

 [deSchedule s] Unschedule the script s from execution.

 [scheduleScript s serv d] after a call to this function, the script
 determined by the file s on server serv is scheduled to execute every d
 seconds. Usually, calls to the scheduleScript function appears in the
 initialization script ../web_sys/init.sml to setup scheduled
 execution. If serv is NONE localhost is used as server.

 [scheduleDaily s serv {hour,minute}] after a call to this
 function, the script determined by the file s on server serv is
 scheduled to execute every day at the specified time (hour and
 minute). The hour can be an integer from 0 to 23, and the minute an
 integer from 0 to 59. If serv is NONE localhost is used as server.

 [scheduleWeekly s serv {day,hour,minute}] after a call to this
 function, the script determined by the file s on server serv is
 scheduled to execute every week at the specified time (day, hour, and
 minute). The day can be an integer from 0 to 6, where 0 represents
 Sunday. The hour can be an integer from 0 to 23, and the minute an
 integer from 0 to 59. If serv is NONE localhost is used as server.

 [exit()] terminates the script by raising the exception
 Interrupt, which is silently caught by the SMLserver module. Other
 uncaught exceptions are logged in the log file.

*)
