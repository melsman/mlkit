signature NS = sig

  (* logging *)
  eqtype LogSeverity    
  val Notice  : LogSeverity     
  val Warning : LogSeverity
  val Error   : LogSeverity
  val Fatal   : LogSeverity
  val Bug     : LogSeverity
  val Debug   : LogSeverity
  val log     : LogSeverity * string -> unit

  (* status codes *)
  eqtype status
  val OK       : status
  val ERROR    : status
  val END_DATA : status

  (* various functions *)
  type quot = Quot.quot
  val return         : quot -> status    
  val write          : quot -> status
  val returnHeaders  : unit -> unit
  val returnRedirect : string -> status
  val returnFileMime : string -> string -> status
  val returnFile     : string -> status
  val getMimeType    : string -> string
  val getHostByAddr  : string -> string option 
  val encodeUrl      : string -> string
  val decodeUrl      : string -> string
  val buildUrl       : string -> (string * string) list 
                       -> string
  val fetchUrl       : string -> string option
  val exit           : unit -> 'a
  val registerTrap   : string -> unit
  val scheduleScript : string -> int -> unit
  val scheduleDaily  : string -> {hour:int, minute:int} 
                       -> unit

  (* sub-structures *)
  structure Set      : NS_SET
  structure Conn     : NS_CONN where type status = status 
				 and type set = Set.set
  structure Cookie   : NS_COOKIE
  structure Cache    : NS_CACHE
  structure Info     : NS_INFO
  structure Mail     : NS_MAIL
  structure DbOra    : NS_DB where type set = Set.set
  structure DbPg     : NS_DB where type set = Set.set
  structure DbMySQL  : NS_DB where type set = Set.set
end

(*
 [LogSeverity] abstract type of log severity.

 [Notice] something interesting occurred.

 [Warning] maybe something bad occurred.

 [Error] something bad occurred.

 [Fatal] something extremely bad occurred. The server shuts 
 down after logging this message.

 [Bug] something occurred that implies there is a bug in 
 your code.

 [Debug] if the server is in Debug mode, the message is 
 printed. Debug mode is specified in the [ns/parameters] 
 section of the configuration file. If the server is not in 
 debug mode, the message is not printed.

 [log (ls,s)] write the string s to the log file with log 
 severity ls.

 [status] abstract type of status code returned by
 functions.

 [OK] status code indicating success.

 [ERROR] status code indicating failure.

 [END_DATA] status code indicating end of data.

 [quot] type of quotations.

 [return q] sends string q to browser with status code 
 200, adding HTTP headers. Returns OK on success and ERROR 
 on failure.
 
 [write q] sends string q to browser. Returns OK on success 
 and ERROR on failure.

 [returnHeaders()] sends HTTP headers to browser.

 [returnRedirect loc] sends a redirection HTTP response to 
 location loc. Returns OK on success and ERROR on failure.

 [returnFileMime mimetype file] returns the entire contents 
 of the given file to the client. In addition to setting the 
 HTTP status response line to 200 and the Content-Type header 
 from the given parameter, the function also uses the stat 
 system call to generate the appropriate Last-Modified and
 Content-Length headers. The function returns a status of OK
 or ERROR.

 [returnFile file] as returnFileMime, but gets the 
 Content-Type (mimetype) argument from calling the function
 getMimeType with the given file as parameter.

 [getMimeType f] guesses the Mime type based on the 
 extension of the filename f. Case is ignored. The return 
 value is of the form "text/html".

 [getHostByAddr ip] converts a numeric IP address ip into a 
 host name. If no name can be found, NONE is returned. 
 Because the response time of the Domain Name Service can be 
 slow, this function may significantly delay the response to 
 a client.

 [encodeUrl s] returns an encoded version of the argument s 
 as URL query data. All characters except the alphanumerics 
 are encoded as specified in RFC1738, Uniform Resource 
 Locators. This function can be used to append arguments to 
 a URL as query data following a `?'.

 [decodeUrl s] decodes data s that was encoded as URL query 
 data. The decoded data is returned.

 [buildUrl u l] constructs a link to the URL u with the form 
 variable pairs l appended to u?, delimited by &, and with 
 the form values URL encoded.

 [fetchUrl u] fetches a remote URL u; connects the Web 
 server to another HTTP Web server and requests the 
 specified URL. The URL must be fully qualified. Currently, 
 the function cannot handle redirects or requests for any 
 protocol except HTTP. Returns NONE if no page is found.

 [exit()] terminates the script by raising the exception
 Interrupt, which is silently caught by the SMLserver module 
 (other uncaught exceptions are logged in the server.log 
 file).

 [registerTrap p] after a call to this function, requests for
 files that matches the path p, which may contain globs, are 
 trapped. The effect of a file being trapped is that the 
 script ../sys/trap.sml is executed instead. Usually, calls 
 to the registerTrap function appears in the initialization 
 script ../sys/init.sml to control access to web content.

 [scheduleScript f d] after a call to this function, the 
 script determined by the file f is scheduled to execute
 every d seconds. Usually, calls to the scheduleScript 
 function appears in the initialization script 
 ../sys/init.sml to setup scheduled execution.

 [scheduleDaily f {hour,minute}] after a call to this 
 function, the script determined by the file f is scheduled
 to execute every day at the specified time (hour and 
 minute). The hour can be an integer from 0 to 23, and the 
 minute an integer from 0 to 59.

 [scheduleWeekly f {day,hour,minute}] after a call to this 
 function, the script determined by the file f is scheduled
 to execute every week at the specified time (day, hour, and 
 minute). The day can be an integer from 0 to 6, where 0 
 represents Sunday. The hour can be an integer from 0 to 23, 
 and the minute an integer from 0 to 59.

*)
